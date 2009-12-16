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
import os
from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer

class Handler(BaseHTTPRequestHandler):

    def do_GET(self):
        self.send_error(404, "Not Found: %s" % self.path)

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
            f = tempfile.NamedTemporaryFile(delete=False, suffix='.txt')
            f.write(body)
            f.close()

            # spawn editor...
            print "Spawning editor... ", f.name

            p = subprocess.Popen(["/usr/bin/emacsclient", f.name], close_fds=True)

            # hold connection open until editor finishes
            p.wait()

            self.send_response(200)
            self.end_headers()

            f = file(f.name, 'r')
            s = f.read()
            f.close()
            try:
                os.unlink(fname)
            except :
                pass

            self.wfile.write(s)
        except :
            self.send_error(404, "Not Found: %s" % self.path)
    
def main():
    import platform
    t = platform.python_version_tuple()
    if int(t[0]) == 2 and int(t[1]) < 6:
        print "Python 2.6+ required"
        # uses tempfile.NamedTemporaryFile delete param
        return
    try:
        httpserv = HTTPServer(('localhost', 9292), Handler)
        httpserv.table = {}
        httpserv.serve_forever()
    except KeyboardInterrupt:
        httpserv.socket.close()

if __name__ == '__main__':
    main()

