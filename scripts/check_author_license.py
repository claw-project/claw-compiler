#!/usr/bin/env python
# -*- coding: utf-8 -*-
# This file is released under terms of BSD license
# See LICENSE file for more information
# @author Mikhail Zhigun

from __future__ import print_function
import argparse
import os
import sys
from os.path import join as join_path

LICENSE_LINES = ('This file is released under terms of BSD license',
                 'See LICENSE file for more information')


class CommentStyle:
    C = 0
    XML = 1
    SH = 2


CS = CommentStyle
EOL = '\n'


def comment_lines(comment_style, lines):
    start = {CS.C: '/*', CS.XML: '<!--', CS.SH: '#'}[comment_style]
    end = {CS.C: '*/', CS.XML: '-->', CS.SH: '#'}[comment_style]
    line = {CS.C: ' * ', CS.XML: ' ', CS.SH: '# '}[comment_style]
    lines = start + EOL + EOL.join([line + l for l in lines]) + EOL + end + EOL
    return lines


CS_BY_EXT = {'.java': CS.C, '.sh': CS.SH, '.xml': CS.XML, '.xsd': CS.XML, '.g4': CS.C, '.py': CS.SH}
FILE_EXTENSIONS = {'.java', '.sh', '.xml', '.xsd', '.g4'}#, '.py'}
IGNORE_REL_PATHS = {'test', 'driver/tests/res', 'driver/unittests/res', 'cx2t/unittest/data'}
MAX_HEADER_LINES = 50

THIS_DIR_PATH = os.path.dirname(os.path.realpath(__file__))
ROOT_DIR_PATH = join_path(THIS_DIR_PATH, '..')


class FileVisitor:
    @property
    def no_license(self):
        return self.__no_license

    @property
    def no_author(self):
        return self.__no_author

    def __init__(self, args):
        self.args = args
        self.__no_license = []
        self.__no_author = []

    def visit_dir(self, dir_abs_path, dir_rel_path):
        if dir_abs_path.startswith('.'):
            return
        if dir_rel_path in IGNORE_REL_PATHS:
            return
        for f in os.listdir(dir_abs_path):
            if not f.startswith('.'):
                f_abs_path = os.path.normpath(join_path(dir_abs_path, f))
                f_rel_path = join_path(dir_rel_path, f)
                if os.path.isdir(f_abs_path):
                    self.visit_dir(f_abs_path, f_rel_path)
                else:
                    self.visit_file(f_abs_path, f_rel_path, f, os.path.splitext(f)[1])

    def in_lines(self, lines, s):
        for l in lines:
            if s in l:
                return True
        return False

    def remove_copyright(self, abs_path):
        with open(abs_path) as f:
            lines = f.readlines()
        header_lines = lines[:min(len(lines), MAX_HEADER_LINES)]
        if self.in_lines(header_lines, '@copyright'):
            n = len(header_lines)
            for i in range(0, n):
                if '@copyright' in lines[i]:
                    del lines[i]
                    break
            with open(abs_path, 'w') as f:
                f.writelines(lines)

    def visit_file(self, abs_path, rel_path, filename, ext):
        if ext not in FILE_EXTENSIONS:
            return
        if p_args.remove_copyright:
            self.remove_copyright(abs_path)
        comment_style = CS_BY_EXT[ext]
        lines = None
        with open(abs_path) as f:
            lines = f.readlines()
        header_lines = lines[:min(len(lines), MAX_HEADER_LINES)]
        has_license = all([self.in_lines(header_lines, s) for s in LICENSE_LINES])
        if not has_license:
            self.__no_license.append(abs_path)
        has_author = self.in_lines(header_lines, 'author')
        if not has_author:
            self.__no_author.append(abs_path)
        add_lines = []
        if not has_license and p_args.add_license:
            add_lines += LICENSE_LINES
        if not has_author and p_args.add_author is not None:
            add_lines += ['@author: ' + p_args.add_author]
        if len(add_lines) > 0:
            header = comment_lines(comment_style, add_lines)
            lines = [header] + lines
            with open(abs_path, 'w') as f:
                f.writelines(lines)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Check source files for presence of license header and author')
    parser.add_argument('--add-author', type=str, help='Name of the author')
    parser.add_argument('--add-license', action='store_true', help='Add license header if missing')
    parser.add_argument('--remove-copyright', action='store_true', help='Remove copyright line')
    parser.add_argument('--dir', type=str, help='Path to top directory')
    p_args = parser.parse_args()
    fv = FileVisitor(p_args)
    top_dir_path = p_args.dir
    if top_dir_path is None:
        top_dir_path = ROOT_DIR_PATH
    fv.visit_dir(top_dir_path, '')
    SUCCESS_RET_CODE = 0
    FAILURE_RET_CODE = 1
    RET_CODE = SUCCESS_RET_CODE
    if len(fv.no_license) > 0:
        print('%s files with no license header' % len(fv.no_license))
        for f_path in fv.no_license:
            print('\t%s' % f_path)
        if not p_args.add_license:
            RET_CODE = FAILURE_RET_CODE
    if len(fv.no_author) > 0:
        print('%s files with no author header' % len(fv.no_author))
        for f_path in fv.no_author:
            print('\t%s' % f_path)
        if not p_args.add_author:
            RET_CODE = FAILURE_RET_CODE
    sys.exit(RET_CODE)
