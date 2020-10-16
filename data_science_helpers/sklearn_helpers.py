from sklearn import metrics
import matplotlib.axes as axs
import matplotlib.pyplot as plt
import numpy as np

class AccuracyLossSummary(dict):
    accuracy_key = "accuracy"
    loss_key = "0-1 loss"
    total_key = "total"

    def set_total(self, model):
        """
        Interesting things happen when accuracy and 0-1 loss are not equal to 1 in total
        """
        self[model][self.total_key] = self[model][self.accuracy_key] + self[model][self.loss_key]
    
    def add_accuracy_and_loss(self, model, accuracy, loss):
        self[model] = {}
        self[model][self.accuracy_key] = accuracy
        self[model][self.loss_key] = loss
        self.set_total(model) 


def multinomial_AUC(test_Y, test_x, classifier):
    test_Y_list = test_Y.values.tolist()
    test_x_list = test_x.values.tolist()

    areas_under_curves = {}
    
    for class_name in set(test_Y_list):
        reduced_test_Y = []
        reduced_test_x = []
        positive_label = 1
        negative_label = 0

        """
        For the purposes of reporting the results of the ROC, we need to obtain new predictions for binary labels and then re-label the predictions, but with positives as 1 and negative as 0. This is because the roc_curve function and auc functions support only numberic, binary data.
        """
        for i, label in enumerate(test_Y_list):
            if label == class_name:
                reduced_test_Y.append(1)
            else:
                reduced_test_Y.append(0)
            reduced_test_x.append(test_x_list[i])

        # One variable's results will be the same as previous results using the same trained classifier
        pred_y = classifier.predict(reduced_test_x)
        binary_y = []

        for i, prediction in enumerate(pred_y):
            if prediction == class_name:
                binary_y.append(positive_label)
            else:
                binary_y.append(negative_label)

        # To check individual results are the same as all results
        """
        print("Metrics for {}".format(class_name))
        print(metrics.classification_report(reduced_test_Y, binary_y))
        """

        fpr, tpr, _ = metrics.roc_curve(reduced_test_Y, binary_y)
        auc = metrics.auc(fpr, tpr)
        areas_under_curves[class_name] = auc

    return areas_under_curves

def weighted_arithmetic_mean(data, weights=None):
    data_length = len(data)
    if weights is None:
        weights = [1/data_length] * data_length
    assert data_length == len(weights)
    total = 0
    for datum, weight in zip(data, weights):
        total += datum * weight
    return total / sum(weights)

def add_AUC_to_report(auc, report, class_distributions=None):
    auc_list = []
    distribution_list = []
    for key in auc.keys():
        if key not in report:
            print("key in AUC, {}, not in report".format(key))
        report[key]["AUC"] = auc[key]
        auc_list.append(auc[key])
        distribution_list.append(class_distributions[key])
    report["macro avg"]["AUC"] = weighted_arithmetic_mean(auc_list, None) 
    report["weighted avg"]["AUC"] = weighted_arithmetic_mean(auc_list, distribution_list)

def plot_class_frequency(training_labels):
    x_axis = set(training_labels)
    y_axis = []
    for tick in x_axis:
        y_axis.append(list(training_labels).count(tick))
    total = sum(y_axis)
    for i, value in enumerate(y_axis):
        y_axis[i] = value/total
    pos = np.arange(len(x_axis))
    plt.bar(pos, y_axis)
    plt.xticks(pos, x_axis)

def class_distribution_dict(training_labels):
    unique_labels = set(training_labels)
    label_counts = []
    for label in unique_labels:
        label_counts.append(list(training_labels).count(label))
    total = sum(label_counts)
    labels_summary = {}
    for label, count in zip(unique_labels, label_counts):
        labels_summary[label] = count / total
    return labels_summary

def add_report_to_weighted_averages_summary(weighted_averages_summary, report, model_name):
    weighted_averages_summary[model_name] = report["weighted avg"]
    weighted_averages_summary[model_name]["accuracy"] = report["accuracy"]

def add_report_to_attribute_summary(attribute_summary, report, model_name, attribute_name, ignore=["accuracy"]):
    # add the attribute score per model, per class
    attribute_summary[model_name] = {}
    for key in report.keys():
        if key in ignore:
            continue
        attribute_summary[model_name][key] = report[key][attribute_name]
