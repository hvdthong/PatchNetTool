# PatchNet: A Tool for Deep Patch Classfication
The figure belows illustrate our tool:

![The overral design of PatchNet](./figures/overral_design.JPG)

First time setup
----------------
Please install the neccessary libraries before running our tool:
   
- python 2.7 (https://www.anaconda.com/download/#linux)

- tensorflow 1.4.1 (https://www.tensorflow.org)

- numpy 1.14.3 (https://www.numpy.org)

- scikit-learn 0.19.1 (https://scikit-learn.org/stable/)


Dataset details
----------------

* The dataset is put in the folder ./data containing 82,403 patches. 


Hyperparameters:
----------------
We have five different parameters:

* nfolds: number of folds to do cross-validation
* iters: number of iterations for training NetML
* alpha and beta: control the strength of ridge and Network lasso regularization, respectively. 
* kNN: number of nearest of neighbors 

Running the model
----------------
Simply run this command to train the network: 

	$ python run_NetML.py ./data_example/bug_list.txt ./data_example/method_list.txt ./data_example/features.txt ./data_example/groundtruth.txt 10 30 0.1 0.01 10
	
Note that in this case, nfolds=10, iters=30, alpha=0.1, beta=0.01, and kNN=10

Example output: 
----------------

![Output](./doc/output.png)
