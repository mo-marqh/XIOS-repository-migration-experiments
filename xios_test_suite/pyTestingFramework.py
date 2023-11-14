import copy
import glob
import itertools
import json
import os
import shutil
import subprocess
import unittest


class TestXIOSSuite(unittest.TestCase):
    """
    unittest class to manage each test case
    test cases will be added dynamically from files in the repository

    """
    def _run_mpi_testcase(self, atest_dir, acall):
        """
        run the test using mpiexec within acall
        
        """
        # this call needs to be run from a current working directory
        # where the exe and all relevant config exists.
        this_cwd = os.getcwd()
        os.chdir(atest_dir)
        print('\nrunning test in\n{}'.format(atest_dir), flush=True)
        print(' '.join(acall), flush=True)
        try:
            subprocess.check_call(acall)
        except Exception as e:
            raise e
        finally:
            subprocess.check_call("find . -name '*.nc'", shell=True)
            os.chdir(this_cwd)

    def _checkfile(self, expected_fnames, check_test_dir):
        """
        parse the checkfile and assert that all the expected files have
        been created
        
        """
        actual_fnames = set([os.path.basename(apath) for apath in
                             glob.glob('{}/*.nc'.format(check_test_dir))])
        msg = ('the following files were expected within {} but do not exist'
               '\n{}').format(check_test_dir,
                              ' '.join(expected_fnames - actual_fnames))
        # assert that all the .nc files within checkfile.def have been created
        self.assertTrue(expected_fnames.issubset(actual_fnames), msg)


# test case construction based on existence of folders
# use the location of this file as the root for searching
test_root = os.path.dirname(__file__)



def product_dict(**kwargs):
    """
    helper function to expand out a dictionary into a list of
    dictionaries
    """
    keys = kwargs.keys()
    vals = kwargs.values()
    for instance in itertools.product(*vals):
        yield dict(zip(keys, instance))

# create a new folder for the test runs, deleting any existing content
# from previous runs
trunners = os.path.join(test_root, 'TEST_SUITE', 'run_test_dirs')
if os.path.exists(trunners):
    shutil.rmtree(trunners)

# obtain all folders within TEST_SUITE that begin test_
test_dirs = glob.glob(os.path.join(test_root, 'TEST_SUITE', 'test_*'))

if not os.path.exists(trunners):
    os.mkdir(trunners)

# access the default parameters to share across test cases
with open(os.path.join(test_root, 'TEST_SUITE', 'default_param.json'), 'r') as dparam:
    dparams = json.load(dparam)[0]

for atest_dir in test_dirs:
    # iterate through existing tests to create test run directories
    # and test cases
    with open(os.path.join(atest_dir, 'user_param.json'), 'r') as uparam:

        config_list = []
        tmp_config_list = []
        config_dict = json.load(uparam)
        for i in range(len(config_dict)):
            # expand test case config, into one dict per multi-item
            # e.g. if there are two values for NClients, make 2 list items
            tmp_config_list.extend(list(product_dict(**config_dict[i])))
        for config in tmp_config_list:
            # ensure default params are used, unless overridden by test params
            new_config = copy.copy(dparams)
            new_config.update(config)
            config_list.append(new_config)

    tindex = 0
    for config_dict in config_list:
        # each config_list item is its own test case
        run_test_dir = os.path.join(trunners,
                                    '{}{}'.format(os.path.basename(atest_dir), tindex))

        if not os.path.exists(run_test_dir):
            os.mkdir(run_test_dir)

        with open(os.path.join(test_root, 'TEST_SUITE/iodef.xml'), 'r') as iodef:
            # use iodef.xml as the base, and replace required params
            new_iodef = iodef.read()
            xpattern = 'XIOS::'
            # just take 1st one for now (until run established)

            for param in config_dict.items():
                new_iodef = new_iodef.replace(xpattern + param[0], str(param[1]))
            if xpattern in new_iodef:
                matching_lines = [line for line in new_iodef.split('\n') if xpattern in line]
                # print(matching_lines)
                raise ValueError('params not updated: {}'.format('\n'.join(matching_lines)))
            with open(os.path.join(run_test_dir, 'iodef.xml'), 'w') as test_iodef:
                # new iodef.xml in run_test_dir
                test_iodef.write(new_iodef)

        with open(os.path.join(run_test_dir, "param.def"), "w") as fh:
            # new param.def in run_test_dir
            fh.write("&params_run\n")
            fh.write("duration=\'"+config_dict["Duration"]+"\'\n")
            # fh.write("nb_proc_atm=" + str(config_dict["NumberClients"])+"\n")
            # hold n clients at 1 whilst messaging issue persists
            fh.write("nb_proc_atm="+str(1)+"\n")
            fh.write("/\n")

        #symbolically link required .xml, .nc & .exe into run_test_dir
        if not os.path.exists(os.path.join(run_test_dir, 'context_grid_dynamico.xml')):
            os.symlink(os.path.join(test_root, 'TEST_SUITE', 'context_grid_dynamico.xml'),
                       os.path.join(run_test_dir, 'context_grid_dynamico.xml'))
        if not os.path.exists(os.path.join(run_test_dir, 'dynamico_grid.nc')):
            os.symlink(os.path.join(test_root, 'TEST_SUITE', 'dynamico_grid.nc'),
                       os.path.join(run_test_dir, 'dynamico_grid.nc'))
        if not os.path.exists(os.path.join(run_test_dir, 'generic_testcase.exe')):
            os.symlink(os.path.join(os.path.dirname(test_root), 'bin', 'generic_testcase.exe'),
                       os.path.join(run_test_dir, 'generic_testcase.exe'))
        if not os.path.exists(os.path.join(run_test_dir, 'context_atm.xml')):
            os.symlink(os.path.join(atest_dir, 'context_atm.xml'),
                       os.path.join(run_test_dir, 'context_atm.xml'))
        # hold n processes at 2 whilst messaging issue persists
        # acall = ['mpiexec', '-np', str(config_dict["NumberClients"] + 1), 'generic_testcase.exe']
        acall = ['mpiexec', '-np', '2', 'generic_testcase.exe']

        def make_a_test(the_test_dir, acall):
            """
            function to create a test case, with copies of required information.
            copies and deep copies are needed as teh test_xios function will be run
            within the test case, not in line.
            
            """
            test_dir = copy.copy(the_test_dir)
            call = copy.deepcopy(acall)
            expected_fnames_tmp = set()
            checkfile = os.path.join(atest_dir, 'checkfile.def')
            with open(checkfile) as cfile:
                for line in cfile.readlines():
                    if not line.startswith('#') and line.endswith('.nc\n'):
                        expected_fnames_tmp.add(line.strip())
            expected_fnames = copy.deepcopy(expected_fnames_tmp)
            def test_xios(self):
                """
                the test function which will be added to the test class
                runs the mpiexec on generic_testcase.exe within the run-test_dir
                this needs to be run within the correct folder
                
                """
                self._run_mpi_testcase(test_dir, call)
                self._checkfile(expected_fnames, test_dir)

            return test_xios
        tname = 'test_{}{}'.format(os.path.basename(atest_dir), tindex)
        # add the new test to the TestXIOSSuite class
        setattr(TestXIOSSuite, tname, make_a_test(run_test_dir, acall))
        tindex += 1


if __name__ == '__main__':
    # run the unittest framework
    unittest.main()

