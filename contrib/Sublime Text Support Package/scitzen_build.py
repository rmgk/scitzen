import sublime
import sublime_plugin

import subprocess
import threading
import os

import imghdr
import struct
import json

import tempfile

# global phantoms added by the build command


class ScitzenBuildCommand(sublime_plugin.WindowCommand):

    encoding = 'utf-8'
    killed = False
    proc = None
    panel = None
    panel_lock = threading.Lock()
    phantom_sets = {}
    image_sync_file = tempfile.NamedTemporaryFile()


    def is_enabled(self, lint=False, integration=False, kill=False):
        # The Cancel build option should only be available
        # when the process is still running
        if kill:
            return self.proc is not None and self.proc.poll() is None
        return True

    def get_working_dir(self):
        return self.window.extract_variables()['folder']

    def run(self, lint=False, integration=False, kill=False):

        if kill:
            if self.proc:
                self.killed = True
                self.proc.terminate()
            return

        working_dir = self.get_working_dir()

        with self.panel_lock:
            # Creating the panel implicitly clears any previous contents
            self.panel = self.window.create_output_panel('exec')

            # Enable result navigation. The result_file_regex does
            # the primary matching, but result_line_regex is used
            # when build output includes some entries that only
            # contain line/column info beneath a previous line
            # listing the file info. The result_base_dir sets the
            # path to resolve relative file names against.
            settings = self.panel.settings()
            settings.set(
                'result_file_regex',
                r'at »(...*?):(\d*):(\d*)«'
            )
            # settings.set(
            #     'result_line_regex',
            #     r'^\s+line (\d+) col (\d+)'
            # )
            settings.set('result_base_dir', working_dir)
            settings.set('filepath', working_dir)
            settings.set('word_wrap', True)
            settings.set('line_numbers', False)
            settings.set('gutter', False)
            settings.set('scroll_past_end', False)

            self.window.run_command('show_panel', {'panel': 'output.exec'})

        view = self.window.active_view()
        position = view.sel()[0].end()
        file = view.file_name()
        print("success!", file, position)

        if self.proc is not None:
            self.proc.terminate()
            self.proc = None

        commandstring = ["scitzen",
            # "--sync-file", file, "--sync-position", str(position),
            "--image-file-map", self.image_sync_file.name,
            file]
        print("calling", commandstring)

        self.proc = subprocess.Popen(
            commandstring,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            cwd=working_dir
        )
        threading.Thread(
            target=self.read_handle,
            args=(self.proc.stdout,)
        ).start()

    def read_handle(self, handle):
        chunk_size = 2 ** 13
        out = b''
        while True:
            try:
                data = os.read(handle.fileno(), chunk_size)
                # If exactly the requested number of bytes was
                # read, there may be more data, and the current
                # data may contain part of a multibyte char
                out += data
                if len(data) == chunk_size:
                    continue
                if data == b'' and out == b'':
                    raise IOError('EOF')
                # We pass out to a function to ensure the
                # timeout gets the value of out right now,
                # rather than a future (mutated) version
                self.queue_write(out.decode(self.encoding))
                if data == b'':
                    raise IOError('EOF')
                out = b''
            except (UnicodeDecodeError) as e:
                msg = 'Error decoding output using %s - %s'
                self.queue_write(msg  % (self.encoding, str(e)))
                break
            except (IOError):
                if self.killed:
                    msg = 'Cancelled'
                else:
                    self.display_images()
                    msg = 'Finished'
                self.queue_write('\n[%s]' % msg)
                break

    def queue_write(self, text):
        sublime.set_timeout(lambda: self.do_write(text), 1)

    def do_write(self, text):
        with self.panel_lock:
            self.panel.run_command('append', {'characters': text})


    def load_image_file_map(self):
        working_dir = self.get_working_dir()
        mapping = None
        with open(self.image_sync_file.name, 'r') as f:
            mapping = json.load(f)
        return mapping


    # code based on https://github.com/renerocksai/sublime_zk
    # MIT licensed

    def display_images(self):
        view = self.window.active_view()
        phantom_set = self.phantom_sets.get(view.id(), sublime.PhantomSet(view, "scitzen"))

        def phantom_for(image_desc) -> sublime.Phantom:
            region = sublime.Region(image_desc["start"], image_desc["end"])
            img = image_desc["file"]
            html_format = '''
                <img src="file://{}" style="width: {}; height: {};">
            '''
            sizings = self.good_size(view, img)
            if not sizings: return
            (width, height) = sizings
            html_img = html_format.format(img, width, height)
            return sublime.Phantom(region, html_img, sublime.LAYOUT_BLOCK)
            # view.add_phantom("scitzen", region, html_img, sublime.LAYOUT_BLOCK)

        image_file_map = self.load_image_file_map()
        view.erase_phantoms("scitzen")
        file_name = view.file_name()
        # for desc in image_file_map[file_name]:
        #     phantom_for(desc)
        phantoms = list(filter(lambda x: not not x, map(phantom_for, image_file_map[file_name])))
        phantom_set.update(phantoms)
        self.phantom_sets[view.id()] = phantom_set


    def good_size(self, view, img):
        sizings = self.get_image_size(img)
        if not sizings:
            print("could not get size for " + img)
            return
        (width, height, ttype) = sizings
        ratio = width/height

        print("original", width, height)

        max_width = view.viewport_extent()[0] * 0.7
        max_height = view.viewport_extent()[1] * 0.7
        if width > max_width:
            m = max_width / width
            height *= m
            width = max_width
        if height > max_height:
            m = max_height / height
            width *= m
            height = max_height

        print("limits", max_width, max_height)
        print("max limited", width, height)

        # remsize = 40
        # width = width/max_width * remsize
        # height = width / ratio
        # print("remmed", width, height)
        return (width, height)

    @staticmethod
    def get_image_size(img):
        """
        Determine the image type of img and return its size.
        """
        with open(img, 'rb') as f:
            head = f.read(24)

            # print('head:\n', repr(head))
            if len(head) != 24:
                print("could not open " + img)
                return

            ttype = imghdr.what(img)


            if ttype == 'png':
                check = struct.unpack('>i', head[4:8])[0]
                if check != 0x0d0a1a0a:
                    return
                width, height = struct.unpack('>ii', head[16:24])
            elif ttype == 'gif':
                width, height = struct.unpack('<HH', head[6:10])
            elif ttype == 'jpeg':
                try:
                    f.seek(0)  # Read 0xff next
                    size = 2
                    ftype = 0
                    while not 0xc0 <= ftype <= 0xcf:
                        f.seek(size, 1)
                        byte = f.read(1)
                        while ord(byte) == 0xff:
                            byte = f.read(1)
                        ftype = ord(byte)
                        size = struct.unpack('>H', f.read(2))[0] - 2
                    # SOFn block
                    f.seek(1, 1)  # skip precision byte.
                    height, width = struct.unpack('>HH', f.read(4))
                except Exception:
                    return
            else:
                return
            return width, height, ttype
