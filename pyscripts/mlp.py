__docformat__ = 'restructedtext en'

import cPickle
import gzip
import os
import sys
import time

import numpy

import theano
import theano.tensor as T


from logistic import LogisticRegression, load_data, load_test_data, read_data, print_vals 


class HiddenLayer(object):
    def __init__(self, rng, input, n_in, n_out, W=None, b=None,
                 activation=T.tanh, layername=None):
        """
        Typical hidden layer of a MLP: units are fully-connected and have
        sigmoidal activation function. Weight matrix W is of shape (n_in,n_out)
        and the bias vector b is of shape (n_out,).

        NOTE : The nonlinearity used here is tanh

        Hidden unit activation is given by: tanh(dot(input,W) + b)

        :type rng: numpy.random.RandomState
        :param rng: a random number generator used to initialize weights

        :type input: theano.tensor.dmatrix
        :param input: a symbolic tensor of shape (n_examples, n_in)

        :type n_in: int
        :param n_in: dimensionality of input

        :type n_out: int
        :param n_out: number of hidden units

        :type activation: theano.Op or function
        :param activation: Non linearity to be applied in the hidden
                           layer
        """
        self.input = input

        # `W` is initialized with `W_values` which is uniformely sampled
        # from sqrt(-6./(n_in+n_hidden)) and sqrt(6./(n_in+n_hidden))
        # for tanh activation function
        # the output of uniform if converted using asarray to dtype
        # theano.config.floatX so that the code is runable on GPU
        # Note : optimal initialization of weights is dependent on the
        #        activation function used (among other things).
        #        For example, results presented in [Xavier10] suggest that you
        #        should use 4 times larger initial weights for sigmoid
        #        compared to tanh
        #        We have no info for other function, so we use the same as
        #        tanh.
        if W is None:
            W_values = numpy.asarray(rng.uniform(
                    low=-numpy.sqrt(6. / (n_in + n_out)),
                    high=numpy.sqrt(6. / (n_in + n_out)),
                    size=(n_in, n_out)), dtype=theano.config.floatX)
            if activation == theano.tensor.nnet.sigmoid:
                W_values *= 4

            W = theano.shared(value=W_values, name='W', borrow=True)

        if b is None:
            b_values = numpy.zeros((n_out,), dtype=theano.config.floatX)
            b = theano.shared(value=b_values, name='b', borrow=True)

        self.W = W
        self.b = b

        lin_output = T.dot(input, self.W) + self.b
        self.output = (lin_output if activation is None
                       else activation(lin_output))
        # parameters of the model
        self.params = [self.W, self.b]
	self.layername = layername


class MLP(object):
    """Multi-Layer Perceptron Class

    A multilayer perceptron is a feedforward artificial neural network model
    that has one layer or more of hidden units and nonlinear activations.
    Intermediate layers usually have as activation function thanh or the
    sigmoid function (defined here by a ``SigmoidalLayer`` class)  while the
    top layer is a softmax layer (defined here by a ``LogisticRegression``
    class).
    """

    def __init__(self, rng, input, n_in, n_hidden, n_out, hidden_layers_sizes=[500, 500]):
        """Initialize the parameters for the multilayer perceptron

        :type rng: numpy.random.RandomState
        :param rng: a random number generator used to initialize weights
        :type input: theano.tensor.TensorType
        :param input: symbolic variable that describes the input of the
        architecture (one minibatch)

        :type n_in: int
        :param n_in: number of input units, the dimension of the space in
        which the datapoints lie

        :type n_hidden: int
        :param n_hidden: number of hidden units

        :type n_out: int
        :param n_out: number of output units, the dimension of the space in
        which the labels lie

        """
	#self.x = T.matrix('x')  
        #self.y = T.ivector('y')  

	self.sigmoid_layers = []
        self.n_layers = len(hidden_layers_sizes)
	self.params = []

        # Since we are dealing with a one hidden layer MLP, this will
        # translate into a TanhLayer connected to the LogisticRegression
        # layer; this can be replaced by a SigmoidalLayer, or a layer
        # implementing any other nonlinearity

        for i in xrange(self.n_layers):
            if i == 0:
                input_size = n_in
            else:
                input_size = hidden_layers_sizes[i - 1]
            if i == 0:
                layer_input = input
            else:
                layer_input = self.sigmoid_layers[-1].output

            sigmoid_layer = HiddenLayer(rng=rng, 
                                        input=layer_input,
                                        n_in=input_size,
                                        n_out=hidden_layers_sizes[i],
                                        activation=T.nnet.sigmoid, 
					layername="h"+str(i))

	    self.sigmoid_layers.append(sigmoid_layer)
	    self.params.extend(sigmoid_layer.params)

        # the parameters of the model are the parameters of the two layer it is
        # made out of
        #self.params = self.hiddenLayer.params + self.logRegressionLayer.params

        # The logistic regression layer gets as input the hidden units
        # of the hidden layer
        self.logRegressionLayer = LogisticRegression(
            input=self.sigmoid_layers[-1].output,
            n_in=hidden_layers_sizes[-1],
            n_out=n_out)

	self.params.extend(self.logRegressionLayer.params)

        # L1 norm ; one regularization option is to enforce L1 norm to
        # be small
        self.L1 = sum(map(lambda y: abs(y.W).sum(), self.sigmoid_layers)) \
                + abs(self.logRegressionLayer.W).sum()

        # square of L2 norm ; one regularization option is to enforce
        # square of L2 norm to be small
        self.L2_sqr = sum(map(lambda y: abs(y.W ** 2).sum(), self.sigmoid_layers)) \
                + abs(self.logRegressionLayer.W ** 2).sum()
        #self.L2_sqr = (self.hiddenLayer.W ** 2).sum() \
        #            + (self.logRegressionLayer.W ** 2).sum()

        # negative log likelihood of the MLP is given by the negative
        # log likelihood of the output of the model, computed in the
        # logistic regression layer
        self.negative_log_likelihood = self.logRegressionLayer.negative_log_likelihood
        # same holds for the function computing the number of errors
        self.errors = self.logRegressionLayer.errors


	# clai: Some functions to dump predicted values
	self.get_y_pred = self.logRegressionLayer.get_y_pred

	self.get_p_y_given_x = self.logRegressionLayer.get_p_y_given_x


    def get_ptest(self, y):
	return y

#-----------------------------------------------------------------

    def get_params(self):
        """
            return the network parameters as a dict (for saving values to file)
        """
	params = {}
        for layer in self.sigmoid_layers:
	    params[layer.layername] = {}
            for param in layer.params:                
                params[layer.layername][param.name] = param.get_value()

	params[self.logRegressionLayer.layername] = {}
        for param in self.logRegressionLayer.params:
            params[self.logRegressionLayer.layername][param.name] = param.get_value()
                        
        return params

    # --------------------------------------------------------------------------   
    def set_params(self, params):
        """
            set the network parameters from given dict
        """       
	print "set_params"
	for layer in self.sigmoid_layers:     
		lname = layer.layername 
		print lname
		for name, value in params[lname].items():
			for param in layer.params:            
			    if param.name == name:
				param.set_value(value)

	lname = self.logRegressionLayer.layername 
	print lname
	for name, value in params[lname].items():
		for param in self.logRegressionLayer.params:
			if param.name == name:
				param.set_value(value)     

# --------------------------------------------------------------------------

def load_params(self, path):
	f = file(path, 'r')
	obj = cPickle.load(f)
	f.close()
	return obj

def save_params(self, obj, path):
	f = file(path, 'wb')
	cPickle.dump(obj, f, protocol=cPickle.HIGHEST_PROTOCOL)
	f.close()

# --------------------------------------------------------------------------

def test_mlp(learning_rate=0.1, L1_reg=0.00, L2_reg=0.0001, n_epochs=1000,
             dataset='../data/old.murray.all.pkl.gz', batch_size=1000, n_hidden=500, 
             n_in=180, n_out=2, hidden_layers_sizes=[500,500], outfile="ami-mlp-eval.pkl"):
    """
    Demonstrate stochastic gradient descent optimization for a multilayer
    perceptron

    :type learning_rate: float
    :param learning_rate: learning rate used (factor for the stochastic
    gradient

    :type L1_reg: float
    :param L1_reg: L1-norm's weight when added to the cost (see
    regularization)

    :type L2_reg: float
    :param L2_reg: L2-norm's weight when added to the cost (see
    regularization)

    :type n_epochs: int
    :param n_epochs: maximal number of epochs to run the optimizer

    :type dataset: string
    :param dataset: the path of the MNIST dataset file from
                 http://www.iro.umontreal.ca/~lisa/deep/data/mnist/mnist.pkl.gz


   """
    datasets = load_data(dataset)

    train_set_x, train_set_y, train_set_z = datasets[0]
    valid_set_x, valid_set_y, valid_set_z = datasets[1]
    test_set_x, test_set_y, test_set_z = datasets[2]

    # compute number of minibatches for training, validation and testing
    n_train_batches = train_set_x.get_value(borrow=True).shape[0] / batch_size
    n_valid_batches = valid_set_x.get_value(borrow=True).shape[0] / batch_size
    n_test_batches = test_set_x.get_value(borrow=True).shape[0] / batch_size
    n_test_size = test_set_x.get_value(borrow=True).shape[0] 
    n_dev_size = valid_set_x.get_value(borrow=True).shape[0] 
    n_train_size = train_set_x.get_value(borrow=True).shape[0] 

    print n_train_size

    ######################
    # BUILD ACTUAL MODEL #
    ######################
    print '... building the model'

    # allocate symbolic variables for the data
    index = T.lscalar()  # index to a [mini]batch
    x = T.matrix('x')  # the data is presented as rasterized images
    y = T.ivector('y')  # the labels are presented as 1D vector of
                        # [int] labels
    z = T.ivector('z')  


    rng = numpy.random.RandomState(1234)

    # construct the MLP class
    classifier = MLP(rng=rng, input=x, n_in=n_in,
                     n_hidden=n_hidden, n_out=n_out, hidden_layers_sizes=hidden_layers_sizes)

    # the cost we minimize during training is the negative log likelihood of
    # the model plus the regularization terms (L1 and L2); cost is expressed
    # here symbolically
    cost = classifier.negative_log_likelihood(y) \
         + L1_reg * classifier.L1 \
         + L2_reg * classifier.L2_sqr

    # compiling a Theano function that computes the mistakes that are made
    # by the model on a minibatch
    test_model = theano.function(inputs=[index],
            outputs=classifier.errors(y),
            givens={
                x: test_set_x[index * batch_size:(index + 1) * batch_size],
                y: test_set_y[index * batch_size:(index + 1) * batch_size]})

    validate_model = theano.function(inputs=[index],
            outputs=classifier.errors(y),
            givens={
                x: valid_set_x[index * batch_size:(index + 1) * batch_size],
                y: valid_set_y[index * batch_size:(index + 1) * batch_size]})

    pred_prob_model = theano.function(inputs=[index],
            outputs=[y, classifier.get_y_pred(y), classifier.get_p_y_given_x(y), z],
            givens={
                x: test_set_x[index: (index + 1)],
                y: test_set_y[index: (index + 1)], 
                z: test_set_z[index: (index + 1)]})

    pred_prob_dev = theano.function(inputs=[index],
            outputs=[y, classifier.get_y_pred(y), classifier.get_p_y_given_x(y), z],
            givens={
                x: valid_set_x[index: (index + 1)],
                y: valid_set_y[index: (index + 1)], 
                z: valid_set_z[index: (index + 1)]})

    pred_prob_train = theano.function(inputs=[index],
            outputs=[y, classifier.get_y_pred(y), classifier.get_p_y_given_x(y), z],
            givens={
                x: train_set_x[index: (index + 1)],
                y: train_set_y[index: (index + 1)], 
                z: train_set_z[index: (index + 1)]})



    # the resulting gradients will be stored in a list gparams
    gparams = []
    for param in classifier.params:
        gparam = T.grad(cost, param)
        gparams.append(gparam)

    # specify how to update the parameters of the model as a list of
    # (variable, update expression) pairs
    updates = []
    # given two list the zip A = [a1, a2, a3, a4] and B = [b1, b2, b3, b4] of
    # same length, zip generates a list C of same size, where each element
    # is a pair formed from the two lists :
    #    C = [(a1, b1), (a2, b2), (a3, b3), (a4, b4)]
    for param, gparam in zip(classifier.params, gparams):
        updates.append((param, param - learning_rate * gparam))

    # compiling a Theano function `train_model` that returns the cost, but
    # in the same time updates the parameter of the model based on the rules
    # defined in `updates`
    train_model = theano.function(inputs=[index], outputs=cost,
            updates=updates,
            givens={
                x: train_set_x[index * batch_size:(index + 1) * batch_size],
                y: train_set_y[index * batch_size:(index + 1) * batch_size]})

    ###############
    # TRAIN MODEL #
    ###############
    print '... training'

    # early-stopping parameters
    patience = 10000  # look as this many examples regardless
    patience_increase = 2  # wait this much longer when a new best is
                           # found
    improvement_threshold = 0.995  # a relative improvement of this much is
                                   # considered significant
    validation_frequency = min(n_train_batches, patience / 2)
                                  # go through this many
                                  # minibatche before checking the network
                                  # on the validation set; in this case we
                                  # check every epoch

    best_params = None
    best_validation_loss = numpy.inf
    best_iter = 0
    test_score = 0.
    start_time = time.clock()

    epoch = 0
    done_looping = False

    while (epoch < n_epochs) and (not done_looping):
        epoch = epoch + 1
        for minibatch_index in xrange(n_train_batches):

            minibatch_avg_cost = train_model(minibatch_index)
            # iteration number
            iter = (epoch - 1) * n_train_batches + minibatch_index

            if (iter + 1) % validation_frequency == 0:
                # compute zero-one loss on validation set
                validation_losses = [validate_model(i) for i
                                     in xrange(n_valid_batches)]
                this_validation_loss = numpy.mean(validation_losses)

                print('epoch %i, minibatch %i/%i, validation error %f %%' %
                     (epoch, minibatch_index + 1, n_train_batches,
                      this_validation_loss * 100.))

                # if we got the best validation score until now
                if this_validation_loss < best_validation_loss:
                    #improve patience if loss improvement is good enough
                    if this_validation_loss < best_validation_loss *  \
                           improvement_threshold:
                        patience = max(patience, iter * patience_increase)

                    best_validation_loss = this_validation_loss
                    best_iter = iter

                    # test it on the test set
                    test_losses = [test_model(i) for i
                                   in xrange(n_test_batches)]
                    test_score = numpy.mean(test_losses)

                    print(('     epoch %i, minibatch %i/%i, test error of '
                           'best model %f %%') %
                          (epoch, minibatch_index + 1, n_train_batches,
                           test_score * 100.))

            if patience <= iter:
                    done_looping = True
                    break

    end_time = time.clock()

    print "End training"
    print "Get probabilities for all data"
    print "train"
    train_preds = [pred_prob_train(i) for i in xrange(n_train_size)]
    print "test"
    test_preds = [pred_prob_model(i) for i in xrange(n_test_size)]
    print "dev"
    dev_preds = [pred_prob_dev(i) for i in xrange(n_dev_size)]

    print len(train_preds), len(test_preds), len(dev_preds)

    print "Dump probs"
    save_file = open(outfile, 'wb')
    cPickle.dump([map(lambda y: [y[0][0], y[1][0], y[2][0][1], y[3][0]], dev_preds), 
		map(lambda y: [y[0][0], y[1][0], y[2][0][1], y[3][0]], test_preds),
		map(lambda y: [y[0][0], y[1][0], y[2][0][1], y[3][0]], train_preds)], save_file, -1)
		

    print "Dump params"	
    paramfile = "params-" + outfile 	
    print paramfile
    save_file = open(paramfile, 'wb')
    
    currparams = classifier.get_params()
    
    cPickle.dump(currparams, save_file, -1)


    print(('Optimization complete. Best validation score of %f %% '
           'obtained at iteration %i, with test performance %f %%') %
          (best_validation_loss * 100., best_iter + 1, test_score * 100.))
    print >> sys.stderr, ('The code for file ' +
                          os.path.split(__file__)[1] +
                          ' ran for %.2fm' % ((end_time - start_time) / 60.))


def apply_mlp(dataset='./x.pkl', paramfile="params.pkl", 
		n_hidden=500, n_in=180, n_out=2, hidden_layers_sizes=[500,500],
		outfile="ami-mlp-eval.pkl"):


    [dataset] = load_test_data(dataset)
    test_set_x, test_set_y, test_set_z = dataset

    # compute number of minibatches for training, validation and testing
    n_test_size = test_set_x.get_value(borrow=True).shape[0] 

    print "n_test_size", n_test_size

    # allocate symbolic variables for the data
    index = T.lscalar()  # index to a [mini]batch
    x = T.matrix('x')  # the data is presented as rasterized images
    y = T.ivector('y')  # the labels are presented as 1D vector of
                        # [int] labels
    z = T.ivector('z')  


    rng = numpy.random.RandomState(1234)

    # construct the MLP class
    classifier = MLP(rng=rng, input=x, n_in=n_in,
                     n_hidden=n_hidden, n_out=n_out, hidden_layers_sizes=hidden_layers_sizes)

    print "set params"
    print paramfile
    with open(paramfile, "rb") as f:
	model_params = cPickle.load(f)

    classifier.set_params(model_params)

    #print classifier.get_params()

    # compiling a Theano function that computes the mistakes that are made
    # by the model on a minibatch

    pred_prob_apply = theano.function(inputs=[index],
            outputs=[y, classifier.get_y_pred(y), classifier.get_p_y_given_x(y), z],
            givens={
                x: test_set_x[index: (index + 1)],
                y: test_set_y[index: (index + 1)], 
                z: test_set_z[index: (index + 1)]})


    test_preds = [pred_prob_apply(i) for i in xrange(n_test_size)]
    print "len(test_preds)", len(test_preds)

   # print test_preds
    outvals = map(lambda y: [y[0][0], y[1][0], y[2][0][1], y[3][0]], test_preds)
    print_vals(outvals,  outfile+".eval.txt")

    #save_file = open(outfile, 'wb')
    #cPickle.dump([map(lambda y: [y[0][0], y[1][0], y[2][0][1], y[3][0]], test_preds)],save_file, -1)
				
    print "Model Applied"
    return


if __name__ == '__main__':
	dataset=sys.argv[1]
	outfile=sys.argv[2]
	n_in = int(sys.argv[3])
	testonly = sys.argv[4]
	paramfile = sys.argv[5]
	hlayers = map(lambda y: int(y), sys.argv[6:]) 

	print outfile, dataset, n_in, testonly, paramfile, hlayers
	#test_mlp(dataset='/exports/home/clai/clai/DeepLearningTutorials/data/tf.pkl.gz', hidden_layers_sizes=[500,500,500], n_in=180)

	if testonly == "T": 
		print "test only"
		apply_mlp(dataset=dataset, paramfile=paramfile, hidden_layers_sizes=hlayers, n_in=n_in, outfile=outfile)
	else:
		test_mlp(dataset=dataset, hidden_layers_sizes=hlayers, n_in=n_in, outfile=outfile)
 
