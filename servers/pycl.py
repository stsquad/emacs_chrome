# Copyright (C) 2009  David Hilley <davidhi@cc.gatech.edu>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#
import cgi, urlparse
import subprocess
import tempfile, time
import os, sys, re
from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer

temp_has_delete=True

class Handler(BaseHTTPRequestHandler):
    global temp_has_delete

    def do_GET(self):
	if self.path == '/status':
		self.send_response(200)
		self.send_header('Content-Type', 'text/plain; charset=utf-8')
		self.end_headers()
		self.wfile.write('edit-server is running.\n')
		return
        self.send_error(404, "GET Not Found: %s" % self.path)

    def do_POST(self):
        try:
            (content, params) = cgi.parse_header(self.headers.
                                                 getheader('content-type'))

            clength = 0
            cl = self.headers.getheader('content-length')

            if cl != None:
                clength = int(cl)
            else:
                self.send_response(411)
                self.end_headers()
                return

            body = self.rfile.read(clength)
            print body

            l = [s for s in self.path.split('/') if s]
            print l

            # write text into file
            url = self.headers.getheader('x-url')
            print "url:", url
            prefix = "chrome_"
            if url:
                prefix += re.sub("[^.\w]", "_", re.sub("^.*?//","",url))
            prefix += "_"
            if temp_has_delete==True:
                f = tempfile.NamedTemporaryFile(
                        delete=False, prefix=prefix, suffix='.txt')
                fname = f.name
            else:
                tf = tempfile.mkstemp(prefix=prefix, suffix='.txt')
                f = os.fdopen(tf[0],"w")
                fname = tf[1]

            f.write(body)
            f.close()

            # spawn editor...
            print "Spawning editor... ", fname

            p = subprocess.Popen(["/usr/bin/emacsclient", "-c", fname], close_fds=True)
            #p = subprocess.Popen(["/usr/local/bin/mvim", "--remote-wait", fname], close_fds=True)

            # hold connection open until editor finishes
            rc = p.wait()

            if not rc:
                    self.send_response(200)
                    self.end_headers()

                    f = file(fname, 'r')
                    s = f.read()
                    f.close()
            else:
                    if rc > 0:
                            msg = 'text editor returned %d' % rc
                    elif rc < 0:
                            msg = 'text editor died on signal %d' % -rc
                    self.send_error(404, msg)

            try:
                os.unlink(fname)
            except :
                print "Unable to unlink:", fname
                pass

            self.wfile.write(s)
        except :
            print "Error: ", sys.exc_info()[0]
            self.send_error(404, "Not Found: %s" % self.path)

def main():
    global temp_has_delete
    import platform
    t = platform.python_version_tuple()
    if int(t[0]) == 2 and int(t[1]) < 6:
        temp_has_delete = False;
        print "Handling lack of delete for NamedTemporaryFile:", temp_has_delete
    try:
        httpserv = HTTPServer(('localhost', 9292), Handler)
        httpserv.table = {}
        httpserv.serve_forever()
    except KeyboardInterrupt:
        httpserv.socket.close()

if __name__ == '__main__':
    main()

