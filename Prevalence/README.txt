
Data and R code for the Win-Vector blog post
“Does Balancing Classes Improve Classifier Performance?”
http://www.win-vector.com/blog/2015/02/does-balancing-classes-improve-classifier-performance/

Data from: https://archive.ics.uci.edu/ml/datasets/ISOLET

Source:

Creators: 

Ron Cole and Mark Fanty 
Department of Computer Science and Engineering, 
Oregon Graduate Institute, Beaverton, OR 97006. 
cole '@' cse.ogi.edu, fanty '@' cse.ogi.edu 

Donor: 

Tom Dietterich 
Department of Computer Science 
Oregon State University, Corvallis, OR 97331 
tgd '@' cs.orst.edu


Data Set Information:

This data set was generated as follows.q 150 subjects spoke the name
of each letter of the alphabet twice. Hence, we have 52 training
examples from each speaker. The speakers are grouped into sets of 30
speakers each, and are referred to as isolet1, isolet2, isolet3,
isolet4, and isolet5. The data appears in isolet1+2+3+4.data in
sequential order, first the speakers from isolet1, then isolet2, and
so on. The test set, isolet5, is a separate file.

You will note that 3 examples are missing. I believe they were dropped
due to difficulties in recording.

I believe this is a good domain for a noisy, perceptual task. It is
also a very good domain for testing the scaling abilities of
algorithms. For example, C4.5 on this domain is slower than
backpropagation!

I have formatted the data for C4.5 and provided a C4.5-style names
file as well.


Attribute Information:

The features are described in the paper by Cole and Fanty cited
above. The features include spectral coefficients; contour features,
sonorant features, pre-sonorant features, and post-sonorant
features. Exact order of appearance of the features is not known.


Relevant Papers:

Fanty, M., Cole, R. (1991). Spoken letter recognition. In Lippman,
R. P., Moody, J., and Touretzky, D. S. (Eds). Advances in Neural
Information Processing Systems 3. San Mateo, CA: Morgan Kaufmann.


Dietterich, T. G., Bakiri, G. (1991) Error-correcting output codes: A
general method for improving multiclass inductive learning
programs. Proceedings of the Ninth National Conference on Artificial
Intelligence (AAAI-91), Anaheim, CA: AAAI Press.  

Dietterich, T. G., Bakiri, G. (1994) Solving Multiclass Learning
Problems via Error-Correcting Output Codes. 

