var test_irene_revision_list = ['1797']

var test_irene_info_list = [
        ['1797', 'irene', 'build_X64_IRENE_prod', 'trunk', 'Irene', 'X64_IRENE', 'prod', '/ccc/work/cont003/gen0826/gen0826/XIOS_Test_Suite/XIOS_trunk/build_X64_IRENE_prod'],
        ['1797', 'irene', 'build_X64_IRENE_debug', 'trunk', 'Irene', 'X64_IRENE', 'debug', '/ccc/work/cont003/gen0826/gen0826/XIOS_Test_Suite/XIOS_trunk/build_X64_IRENE_debug']]

var test_irene_1797_X64_IRENE_prod = [
        ['test_scalar_algo', 'config_UsingSrv2=false_NbServers=2_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_scalar_algo', 'config_UsingSrv2=false_NbServers=2_ATMdomain=lmdz', 'atm_output_zoom.nc', 1],
        ['test_scalar_algo', 'config_UsingSrv2=false_NbServers=4_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_scalar_algo', 'config_UsingSrv2=false_NbServers=4_ATMdomain=lmdz', 'atm_output_zoom.nc', 1],
        ['test_scalar_algo', 'config_UsingSrv2=true_NbServers=2_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_scalar_algo', 'config_UsingSrv2=true_NbServers=2_ATMdomain=lmdz', 'atm_output_zoom.nc', 1],
        ['test_scalar_algo', 'config_UsingSrv2=true_NbServers=4_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_scalar_algo', 'config_UsingSrv2=true_NbServers=4_ATMdomain=lmdz', 'atm_output_zoom.nc', 1],
        ['test_grid_algo', 'config_UsingSrv2=false_NbServers=2_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_grid_algo', 'config_UsingSrv2=false_NbServers=2_ATMdomain=lmdz', 'atm_output_zoom.nc', 1],
        ['test_grid_algo', 'config_UsingSrv2=false_NbServers=4_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_grid_algo', 'config_UsingSrv2=false_NbServers=4_ATMdomain=lmdz', 'atm_output_zoom.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=true_NbServers=2_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=true_NbServers=2_ATMdomain=lmdz', 'atm_output_expand.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=true_NbServers=2_ATMdomain=lmdz', 'atm_output_extract.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=true_NbServers=2_ATMdomain=lmdz', 'atm_output_interpolate.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=true_NbServers=2_ATMdomain=lmdz', 'atm_output_zoom.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=true_NbServers=4_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=true_NbServers=4_ATMdomain=lmdz', 'atm_output_expand.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=true_NbServers=4_ATMdomain=lmdz', 'atm_output_extract.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=true_NbServers=4_ATMdomain=lmdz', 'atm_output_interpolate.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=true_NbServers=4_ATMdomain=lmdz', 'atm_output_zoom.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=false_NbServers=2_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=false_NbServers=2_ATMdomain=lmdz', 'atm_output_expand.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=false_NbServers=2_ATMdomain=lmdz', 'atm_output_extract.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=false_NbServers=2_ATMdomain=lmdz', 'atm_output_interpolate.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=false_NbServers=2_ATMdomain=lmdz', 'atm_output_zoom.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=false_NbServers=4_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=false_NbServers=4_ATMdomain=lmdz', 'atm_output_expand.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=false_NbServers=4_ATMdomain=lmdz', 'atm_output_extract.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=false_NbServers=4_ATMdomain=lmdz', 'atm_output_interpolate.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=false_NbServers=4_ATMdomain=lmdz', 'atm_output_zoom.nc', 1],
        ['test_function', 'config_UsingSrv2=true_NbServers=4_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_function', 'config_UsingSrv2=true_NbServers=2_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_function', 'config_UsingSrv2=false_NbServers=4_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_function', 'config_UsingSrv2=false_NbServers=2_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_axis_algo', 'config_UsingSrv2=true_NbServers=2_ATMdomain=lmdz', 'atm_output_zoom.nc', 1],
        ['test_axis_algo', 'config_UsingSrv2=true_NbServers=4_ATMdomain=lmdz', 'atm_output_zoom.nc', 1]]


var test_irene_1797_X64_IRENE_debug = [
        ['test_scalar_algo', 'config_UsingSrv2=false_NbServers=2_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_scalar_algo', 'config_UsingSrv2=false_NbServers=2_ATMdomain=lmdz', 'atm_output_zoom.nc', 1],
        ['test_scalar_algo', 'config_UsingSrv2=false_NbServers=4_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_scalar_algo', 'config_UsingSrv2=false_NbServers=4_ATMdomain=lmdz', 'atm_output_zoom.nc', 1],
        ['test_scalar_algo', 'config_UsingSrv2=true_NbServers=2_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_scalar_algo', 'config_UsingSrv2=true_NbServers=2_ATMdomain=lmdz', 'atm_output_zoom.nc', 1],
        ['test_scalar_algo', 'config_UsingSrv2=true_NbServers=4_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_scalar_algo', 'config_UsingSrv2=true_NbServers=4_ATMdomain=lmdz', 'atm_output_zoom.nc', 1],
        ['test_grid_algo', 'config_UsingSrv2=false_NbServers=2_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_grid_algo', 'config_UsingSrv2=false_NbServers=2_ATMdomain=lmdz', 'atm_output_zoom.nc', 1],
        ['test_grid_algo', 'config_UsingSrv2=false_NbServers=4_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_grid_algo', 'config_UsingSrv2=false_NbServers=4_ATMdomain=lmdz', 'atm_output_zoom.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=true_NbServers=2_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=true_NbServers=2_ATMdomain=lmdz', 'atm_output_expand.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=true_NbServers=2_ATMdomain=lmdz', 'atm_output_extract.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=true_NbServers=2_ATMdomain=lmdz', 'atm_output_interpolate.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=true_NbServers=2_ATMdomain=lmdz', 'atm_output_zoom.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=true_NbServers=4_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=true_NbServers=4_ATMdomain=lmdz', 'atm_output_expand.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=true_NbServers=4_ATMdomain=lmdz', 'atm_output_extract.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=true_NbServers=4_ATMdomain=lmdz', 'atm_output_interpolate.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=true_NbServers=4_ATMdomain=lmdz', 'atm_output_zoom.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=false_NbServers=2_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=false_NbServers=2_ATMdomain=lmdz', 'atm_output_expand.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=false_NbServers=2_ATMdomain=lmdz', 'atm_output_extract.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=false_NbServers=2_ATMdomain=lmdz', 'atm_output_interpolate.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=false_NbServers=2_ATMdomain=lmdz', 'atm_output_zoom.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=false_NbServers=4_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=false_NbServers=4_ATMdomain=lmdz', 'atm_output_expand.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=false_NbServers=4_ATMdomain=lmdz', 'atm_output_extract.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=false_NbServers=4_ATMdomain=lmdz', 'atm_output_interpolate.nc', 1],
        ['test_domain_algo', 'config_UsingSrv2=false_NbServers=4_ATMdomain=lmdz', 'atm_output_zoom.nc', 1],
        ['test_function', 'config_UsingSrv2=true_NbServers=4_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_function', 'config_UsingSrv2=true_NbServers=2_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_function', 'config_UsingSrv2=false_NbServers=4_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_function', 'config_UsingSrv2=false_NbServers=2_ATMdomain=lmdz', 'atm_output.nc', 1],
        ['test_axis_algo', 'config_UsingSrv2=true_NbServers=2_ATMdomain=lmdz', 'atm_output_zoom.nc', 1],
        ['test_axis_algo', 'config_UsingSrv2=true_NbServers=4_ATMdomain=lmdz', 'atm_output_zoom.nc', 1]]


var test_irene_1797_test_scalar_algo_user_params = [
        'NumberServers = 2, 4',
        'UsingServer2 = \'false\', \'true\'',
        'ATMdomain = \'lmdz\'',
        ]

var test_irene_1797_test_grid_algo_user_params = [
        'NumberServers = 2, 4',
        'UsingServer2 = \'false\'',
        'ATMdomain = \'lmdz\'',
        ]

var test_irene_1797_test_domain_algo_user_params = [
        'NumberServers = 2, 4',
        'UsingServer2 = \'true\', \'false\'',
        'ATMdomain = \'lmdz\'',
        ]

var test_irene_1797_test_function_user_params = [
        'NumberServers = 4, 2',
        'UsingServer2 = \'true\', \'false\'',
        'ATMdomain = \'lmdz\'',
        ]

var test_irene_1797_test_axis_algo_user_params = [
        'NumberServers = 2, 4',
        'ATMdomain=\'lmdz\'',
        'UsingServer2 = \'true\'',
        ]

