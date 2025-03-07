# adapted from https://github.com/kristinakobrock/context-shapes-language
import pickle
import numpy as np

### added granularity ad a parameter
def load_accuracies(all_paths, n_runs=5, n_epochs=300, val_steps=10, zero_shot=False, granularities = False, context_unaware=True):
    """ loads all accuracies into a dictionary, val_steps should be set to the same as val_frequency during training
    """
    result_dict = {'train_acc': [], 'val_acc': [], 'test_acc': [], 
                   'coarse_train_acc': [], 'coarse_test_acc': [], 'coarse_val_acc': [],
                   'fine_train_acc': [], 'fine_test_acc': [], 'fine_val_acc': [],
                   'mixed_train_acc' :[], 'mixed_val_acc':[], 'mixed_test_acc':[],
                   'zs_specific_test_acc': [],
                   'zs_generic_test_acc': [], 'zs_acc_objects': [], 'zs_acc_abstraction': [],
                   'cu_train_acc': [], 'cu_val_acc': [], 'cu_test_acc': [], 'cu_zs_specific_test_acc': [],
                   'cu_zs_generic_test_acc': []}

    for path_idx, path in enumerate(all_paths):

        train_accs = []
        val_accs = []
        test_accs = []
        # coarse -> granularity_coarse, fine -> granularity_fine
        coarse_train_accs = []
        coarse_val_accs = []
        coarse_test_accs = []
        fine_train_accs = []
        fine_val_accs = []
        fine_test_accs = []
        mixed_train_acc = []
        mixed_test_acc = []
        mixed_val_acc = []
        zs_accs_objects = []
        zs_accs_abstraction = []
        zs_specific_test_accs = []
        zs_generic_test_accs = []
        cu_train_accs = []
        cu_val_accs = []
        cu_test_accs = []
        cu_zs_specific_test_accs = []
        cu_zs_generic_test_accs = []

        for run in range(n_runs):

            standard_path = path + '/standard/' + str(run) + '/' 
            granularity_coarse_path =  path + '/standard/' + 'granularity_coarse' + '/' + str(run) + '/'
            granularity_fine_path =  path + '/standard/' + 'granularity_fine' + '/' + str(run) + '/'
            zero_shot_path = path + '/standard/zero_shot/'
            context_unaware_path = path + '/context_unaware/' + str(run) + '/'
            cu_zs_path = path + '/context_unaware/zero_shot/'

            # train and validation accuracy

            data = pickle.load(open(standard_path + 'loss_and_metrics.pkl', 'rb'))
            lists = sorted(data['metrics_train0'].items())
            _, train_acc = zip(*lists)
            train_accs.append(train_acc)
            lists = sorted(data['metrics_test0'].items())
            _, val_acc = zip(*lists)
            if len(val_acc) > n_epochs // val_steps:  # old: we had some runs where we set val freq to 5 instead of 10
                val_acc = val_acc[::2]
            val_accs.append(val_acc)
            test_accs.append(data['final_test_acc'])

            ### FOR NON DEFAULT GRANULARITIES
            if granularities and not zero_shot:
                coarse_data = pickle.load(open(granularity_coarse_path + 'loss_and_metrics.pkl', 'rb'))
                lists = sorted(coarse_data['metrics_train0'].items())
                _, coarse_train_acc = zip(*lists)
                coarse_train_accs.append(coarse_train_acc)
                lists = sorted(coarse_data['metrics_test0'].items())  # Change 'data' to 'coarse_data' here
                _, coarse_val_acc = zip(*lists)
                if len(coarse_val_acc) > n_epochs // val_steps:
                    coarse_val_acc = coarse_val_acc[::2]
                coarse_val_accs.append(coarse_val_acc)
                coarse_test_accs.append(coarse_data['final_test_acc'])

                
                fine_data = pickle.load(open(granularity_fine_path + '/loss_and_metrics.pkl', 'rb'))
                lists = sorted(fine_data['metrics_train0'].items())
                _, fine_train_acc = zip(*lists)
                fine_train_accs.append(fine_train_acc)
                lists = sorted(fine_data['metrics_test0'].items())  # Change 'data' to 'fine_data' here
                _, fine_val_acc = zip(*lists)
                fine_val_accs.append(fine_val_acc)
                fine_test_accs.append(fine_data['final_test_acc'])

            if zero_shot:
                 # TODO: implemwnt loading of results for zero shot and fine/coase context settings, if needed for further experiments
                if granularities :
                    raise NotImplementedError

                for cond in ['specific', 'generic']:
                    # zs_accs_objects.append(data['final_test_acc']) # not sure what's the purpose of this

                    # zero shot accuracy (standard)
                    zs_data = pickle.load(
                        open(zero_shot_path + str(cond) + '/' + str(run) + '/loss_and_metrics.pkl', 'rb'))
                    if cond == 'specific':
                        zs_specific_test_accs.append(zs_data['final_test_acc'])
                    else:
                        zs_generic_test_accs.append(zs_data['final_test_acc'])

                    # zero-shot accuracy (context-unaware)
                    if context_unaware:
                        cu_zs_data = pickle.load(
                            open(cu_zs_path + str(cond) + '/' + str(run) + '/loss_and_metrics.pkl', 'rb'))
                        if cond == 'specific':
                            cu_zs_specific_test_accs.append(cu_zs_data['final_test_acc'])
                        else:
                            cu_zs_generic_test_accs.append(cu_zs_data['final_test_acc'])

            # context-unaware accuracy
            if context_unaware:
                cu_data = pickle.load(open(context_unaware_path + 'loss_and_metrics.pkl', 'rb'))
                lists = sorted(cu_data['metrics_train0'].items())
                _, cu_train_acc = zip(*lists)
                if len(cu_train_acc) != n_epochs:
                    print(path, run, len(cu_train_acc))
                    raise ValueError(
                        "The stored results don't match the parameters given to this function. "
                        "Check the number of epochs in the above mentioned runs.")
                cu_train_accs.append(cu_train_acc)
                lists = sorted(cu_data['metrics_test0'].items())
                _, cu_val_acc = zip(*lists)
                # for troubleshooting in case the stored results don't match the parameters given to this function
                if len(cu_val_acc) != n_epochs // val_steps:
                    print(context_unaware_path, len(cu_val_acc))
                    raise ValueError(
                        "The stored results don't match the parameters given to this function. "
                        "Check the above mentioned files for number of epochs and validation steps.")
                if len(cu_val_acc) > n_epochs // val_steps:
                    cu_val_acc = cu_val_acc[::2]
                cu_val_accs.append(cu_val_acc)
                cu_test_accs.append(cu_data['final_test_acc'])
            
        if granularities:
            result_dict['coarse_train_acc'].append(coarse_train_accs)
            result_dict['coarse_val_acc'].append(coarse_val_accs)
            result_dict['coarse_test_acc'].append(coarse_test_accs)
            result_dict['fine_train_acc'].append(fine_train_accs)
            result_dict['fine_val_acc'].append(fine_val_accs)
            result_dict['fine_test_acc'].append(fine_test_accs)
            result_dict['mixed_train_acc'].append(train_accs)
            result_dict['mixed_val_acc'].append(val_accs)
            result_dict['mixed_test_acc'].append(test_accs)

        result_dict['train_acc'].append(train_accs)
        result_dict['val_acc'].append(val_accs)
        result_dict['test_acc'].append(test_accs)
    
        if zero_shot:
            # result_dict['zs_acc_objects'].append(zs_accs_objects)
            # result_dict['zs_acc_abstraction'].append(zs_accs_abstraction)
            result_dict['zs_specific_test_acc'].append(zs_specific_test_accs)
            result_dict['zs_generic_test_acc'].append(zs_generic_test_accs)
            if context_unaware:
                result_dict['cu_zs_specific_test_acc'].append(cu_zs_specific_test_accs)
                result_dict['cu_zs_generic_test_acc'].append(cu_zs_generic_test_accs)
        if context_unaware:
            result_dict['cu_train_acc'].append(cu_train_accs)
            result_dict['cu_val_acc'].append(cu_val_accs)
            result_dict['cu_test_acc'].append(cu_test_accs)

    for key in result_dict.keys():
        result_dict[key] = np.array(result_dict[key])

    return result_dict


def load_entropies(all_paths, n_runs=5, context_unaware=False, length_cost = 0, granularity= 'mixed'):
    """ loads all entropy scores into a dictionary"""

    if context_unaware:
        setting = 'context_unaware'
    elif length_cost == 0.001:
        setting = 'length_cost_001'
    else:
        setting = 'standard'

    result_dict = {'NMI': [], 'effectiveness': [], 'consistency': [],
                   'NMI_hierarchical': [], 'effectiveness_hierarchical': [], 'consistency_hierarchical': [],
                   'NMI_context_dep': [], 'effectiveness_context_dep': [], 'consistency_context_dep': [],
                   'NMI_concept_x_context': [], 'effectiveness_concept_x_context': [],
                   'consistency_concept_x_context': []}

    for path_idx, path in enumerate(all_paths):

        NMIs, effectiveness_scores, consistency_scores = [], [], []
        NMIs_hierarchical, effectiveness_scores_hierarchical, consistency_scores_hierarchical = [], [], []
        NMIs_context_dep, effectiveness_scores_context_dep, consistency_scores_context_dep = [], [], []
        NMIs_conc_x_cont, effectiveness_conc_x_cont, consistency_conc_x_cont = [], [], []

        for run in range(n_runs):
            if granularity == 'mixed':
                standard_path = path + '/' + setting + '/' + str(run) + '/'
                data = pickle.load(open(standard_path + 'entropy_scores.pkl', 'rb'))
            else:
                gran_path =  path + '/' + setting + '/granularity_' + granularity + '/'+ str(run) + '/'
                data = pickle.load(open(gran_path + 'entropy_scores.pkl', 'rb'))

            NMIs.append(data['normalized_mutual_info'])
            effectiveness_scores.append(data['effectiveness'])
            consistency_scores.append(data['consistency'])
            NMIs_hierarchical.append(data['normalized_mutual_info_hierarchical'])
            effectiveness_scores_hierarchical.append(data['effectiveness_hierarchical'])
            consistency_scores_hierarchical.append(data['consistency_hierarchical'])
            NMIs_context_dep.append(data['normalized_mutual_info_context_dep'])
            effectiveness_scores_context_dep.append(data['effectiveness_context_dep'])
            consistency_scores_context_dep.append(data['consistency_context_dep'])
            NMIs_conc_x_cont.append(data['normalized_mutual_info_concept_x_context'])
            effectiveness_conc_x_cont.append(data['effectiveness_concept_x_context'])
            consistency_conc_x_cont.append(data['consistency_concept_x_context'])

        result_dict['NMI'].append(NMIs)
        result_dict['consistency'].append(consistency_scores)
        result_dict['effectiveness'].append(effectiveness_scores)
        result_dict['NMI_hierarchical'].append(NMIs_hierarchical)
        result_dict['consistency_hierarchical'].append(consistency_scores_hierarchical)
        result_dict['effectiveness_hierarchical'].append(effectiveness_scores_hierarchical)
        result_dict['NMI_context_dep'].append(NMIs_context_dep)
        result_dict['consistency_context_dep'].append(consistency_scores_context_dep)
        result_dict['effectiveness_context_dep'].append(effectiveness_scores_context_dep)
        result_dict['NMI_concept_x_context'].append(NMIs_conc_x_cont)
        result_dict['consistency_concept_x_context'].append(consistency_conc_x_cont)
        result_dict['effectiveness_concept_x_context'].append(effectiveness_conc_x_cont)

    for key in result_dict.keys():
        result_dict[key] = np.array(result_dict[key])

    return result_dict


# Mu and goodman:

def load_accuracies_mu_and_goodman(all_paths, n_runs=5, n_epochs=300, val_steps=10, zero_shot=True):
    """ loads all mu and goodman accuracies into a dictionary, val_steps should be set to the same as val_frequency
    during training
    """
    result_dict = {'zs_specific_test_acc': [],
                   'zs_generic_test_acc': []}

    for path_idx, path in enumerate(all_paths):

        zs_specific_test_accs = []
        zs_generic_test_accs = []

        for run in range(n_runs):

            mu_and_goodman_path = path + '/mu_and_goodman/zero_shot/'

            if zero_shot:
                for cond in ['specific', 'generic']:

                    # zero shot accuracy (standard)
                    zs_data = pickle.load(
                        open(mu_and_goodman_path + str(cond) + '/' + str(run) + '/loss_and_metrics.pkl', 'rb'))
                    if cond == 'specific':
                        zs_specific_test_accs.append(zs_data['final_test_acc'])
                    else:
                        zs_generic_test_accs.append(zs_data['final_test_acc'])

        if zero_shot:
            result_dict['zs_specific_test_acc'].append(zs_specific_test_accs)
            result_dict['zs_generic_test_acc'].append(zs_generic_test_accs)

    for key in result_dict.keys():
        result_dict[key] = np.array(result_dict[key])

    return result_dict
