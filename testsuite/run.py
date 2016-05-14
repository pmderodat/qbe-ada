#! /usr/bin/env python3

import os
import os.path
import subprocess


QBE_ROOT_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
TESTS_DIR = os.path.join(QBE_ROOT_DIR, 'testsuite', 'tests')
TMP_DIR = os.path.join(QBE_ROOT_DIR, 'testsuite', 'tmp')


def setenv():
    """
    Make the QBE Ada library available to GPRbuild.
    """
    project_path = os.environ.get('GPR_PROJECT_PATH', None)
    project_path = ('{}{}{}'.format(QBE_ROOT_DIR, os.path.pathsep, project_path)
                    if project_path else
                    QBE_ROOT_DIR)
    os.environ['GPR_PROJECT_PATH'] = project_path


def find_testcases():
    """
    Generator that yields path name for all testcases.

    These paths are relative to TESTS_DIR.
    """
    for dirpath, dirnames, filenames in os.walk(TESTS_DIR):
        for fn in filenames:
            if fn.endswith('.adb'):
                yield os.path.join(dirpath, fn)


def testcase_name(tc_path):
    return os.path.splitext(os.path.basename(tc_path))[0]


def build_testcases(testcases):
    """
    Build all testcases so they are ready to run.
    """

    source_dirs = {os.path.dirname(tc) for tc in testcases}
    project_file = os.path.join(TMP_DIR, 'qbe_testcases.gpr')

    # Create all the temporary directories
    if not os.path.exists(TMP_DIR):
        os.mkdir(TMP_DIR)
    for tc in testcases:
        tc_dir = os.path.join(TMP_DIR, testcase_name(tc))
        if not os.path.exists(tc_dir):
            os.mkdir(tc_dir)

    # Create a project file to build all testcase drivers
    def format_string_list(str_list):
        return ', '.join('"{}"'.format(s) for s in str_list)

    with open(project_file, 'w') as f:
        f.write(
'''with "qbe.gpr";

project QBE_Testcases is

    for Languages use ("Ada");
    for Source_Dirs use ({});
    for Main use ({});
    for Object_Dir use "obj";

    package Compiler is
        for Default_Switches ("Ada") use ("-O0", "-g");
    end Compiler;

end QBE_Testcases;
'''.format(format_string_list(source_dirs),
           format_string_list(os.path.basename(tc) for tc in testcases)))

    # Build testcase drivers and move each to their testcases directory
    subprocess.check_call(['gprbuild', '-j0', '-p', '-q',
                           '-P{}'.format(project_file)])
    for tc in testcases:
        tc_name = testcase_name(tc)
        os.rename(os.path.join(TMP_DIR, 'obj', tc_name),
                  os.path.join(TMP_DIR, tc_name, tc_name))


def run_testcase(tc):
    """
    Run a testcase.

    Return a string as an error message if there is an error. Return None
    otherwise (i.e. if test is successful).
    """

    tc_name = testcase_name(tc)
    tc_dir = os.path.join(TMP_DIR, tc_name)
    tc_output = os.path.join(os.path.dirname(tc), '{}.out'.format(tc_name))

    if not os.path.exists(tc_output):
        return '{}.out is missing'.format(tc_name)

    # Determine what to do from a specially formatted comment in the testcase
    # source file.
    with open(tc, 'r') as f:
        directive = None

        for line in f:
            line = line.strip()
            if line.startswith('--#'):
                directive = line[3:]
                break
        else:
            return 'testcase directive is missing'
    if directive not in ('check_output', 'exec_gen'):
        return 'invalid testcase directive: {}'.format(directive)

    # Generate a QBE IR source file running the testcase driver
    actual_output_file = os.path.join(tc_dir, 'program.qbe')
    with open(actual_output_file, 'w') as f:
        try:
            subprocess.check_call([os.path.join('.', tc_name)],
                                  stdout=f, cwd=tc_dir)
        except subprocess.CalledProcessError as exc:
            return str(exc)

    # If asked to, turn the generated QBE IR into a standalone program and
    # execute it.
    if directive == 'exec_gen':
        qbe_input = actual_output_file
        gcc_input = os.path.join(tc_dir, 'program.s')
        executable = os.path.join(tc_dir, 'program')
        actual_output_file = os.path.join(tc_dir, 'program.out')

        try:
            subprocess.check_call(['qbe', '-o', gcc_input, qbe_input],
                                  cwd=tc_dir)
            subprocess.check_call(['gcc', '-o', executable, gcc_input],
                                  cwd=tc_dir)
            with open(actual_output_file, 'w') as f:
                subprocess.check_call([os.path.join('.', executable)],
                                      cwd=tc_dir, stdout=f)
        except subprocess.CalledProcessError as exc:
            return str(exc)

    # Compare the actual output and the expected one
    with open(tc_output, 'r') as f:
        expected_output = f.readlines()
    with open(actual_output_file, 'r') as f:
        actual_output = f.readlines()
    if expected_output != actual_output:
        return 'output mismatch'


def main():
    testcases = sorted(find_testcases())
    build_testcases(testcases)
    for tc in testcases:
        tc_name = testcase_name(tc)
        error = run_testcase(tc)
        if error:
            print('\x1b[31mFAIL\x1b[0m {}: {}'.format(tc_name, error))
        else:
            print('\x1b[32mOK\x1b[0m   {}'.format(tc_name))


if __name__ == '__main__':
    setenv()
    main()
