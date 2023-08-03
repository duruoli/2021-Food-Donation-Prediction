# 2021-Food-Donation-Prediction
**Keywords**: Time Series Prediction, Machine Learning, Food Insecurity


## Research Name
Predict In-Kind Food Donation of the Food Bank at Central Eastern North Carolina (FBCENC) via ML Methods

## Institution
NC State University the Gears Program

## Abstract
The distribution of in-kind food donations to the food banks can be hard due to the donations' inconsistent and volatile features. Building time series prediction models with the past data is helpful. However, to make the models robust and reliable enough, we need to choose the methods carefully. In this work, I choose different methods including classical ones like ARIMA, ETS, SVR and more advanced ones like BSTS and LSTM. My goal is to find the best-performed prediction approach for the in-kind donations of the Food Bank at Central Eastern North Carolina (FBCENC). I take 12-month history data as training data to make one-step prediction. The results indicate that among all the models, EST performs the best. Results on BSTS model show that additional economic indicators are correlated with the donation amount, but they couldn't improve the models' performance significantly. 


## Things Done
### 1.Read paper & Search background information
Background information: special Germany food banks (government order large retail business to sign contracts with food banks to donate->what retailers donate is largely related to their retail trade)

Paper: https://www.mubucm.com/doc/7I9oA5nwWWf (mind map based on the paper)

### 2.Learn new methods  
#### 1)BSTS

Learn tutorials:

[1]https://www.unofficialgoogledatascience.com/2017/07/fitting-bayesian-structural-time-series.html

[2]https://rpubs.com/osazuwa/bsts

[3]http://oliviayu.github.io/post/2019-03-21-bsts/

[4]Authorial R package introductions: predict.bsts; bsts.prediction.error

#### 2)Mixture of Experts
Papers:

[1] J. D. Wichard and M. Ogorzalek, "Time series prediction with ensemble models," 2004 IEEE International Joint Conference on Neural Networks (IEEE Cat. No.04CH37541), 2004, pp. 1625-1630 vol.2, doi: 10.1109/IJCNN.2004.1380203.

[2] Huerta, Gabriel, Wenxin Jiang, and Martin A. Tanner. "Time series modeling via hierarchical mixtures." Statistica Sinica (2003): 1097-1118.

[3] W. J. Puma-Villanueva, C. A. M. Lima, E. P. dos Santos and F. J. Von Zuben, "Mixture of heterogeneous experts applied to time series: a comparative study," Proceedings. 2005 IEEE International Joint Conference on Neural Networks, 2005., 2005, pp. 1160-1165 vol. 2, doi: 10.1109/IJCNN.2005.1556017.(waiting)

Tutorials:

[1]https://www.commonlounge.com/discussion/9331c0d004704e89bd4d1da08fd7c7bc

[2]https://github.com/eminorhan/mixture-of-experts

[3]https://machinelearningmastery.com/mixture-of-experts/

[4]https://youtu.be/d_GVvIBlWtI  (Lecture 10.2: Mixtures of Experts [Neural Networks for Machine Learning])

#### 3)LSTM
Paper:

[1]Siami-Namini, Sima, Neda Tavakoli, and Akbar Siami Namin. "A comparison of ARIMA and LSTM in forecasting time series." 2018 17th IEEE International Conference on Machine Learning and Applications (ICMLA). IEEE, 2018.

Tutorial:(learn how to make data stationary, scaled, and turned into supervised structure)

[1]https://machinelearningmastery.com/how-to-develop-lstm-models-for-time-series-forecasting/

[2]https://machinelearningmastery.com/time-series-prediction-lstm-recurrent-neural-networks-python-keras/

[3]https://zhuanlan.zhihu.com/p/86006495 (RNN vs LSTM)

[4]https://machinelearningmastery.com/use-timesteps-lstm-networks-time-series-forecasting/

[5]https://www.kdnuggets.com/2018/11/keras-long-short-term-memory-lstm-model-predict-stock-prices.html

### 3.Do time-series prediction & Comparison

#### 1)Clean raw data 
Filter by year---> sum(na.omit) ---> turn into standard date format(different transformation due to different raw formats)

#### 2)Prediction practise
R; Jupyter notebook (python)

#### 3)Comparison: RMSE, MAE; bsts package function

### 4.Make a poster & Presentation

## Idea Map
1)Read background information about food banks around the world. 

2)Read the paper and find the conclusions very interesting ---> decide to make further work on this topic: different regression components reflecting economic influences (unemployment rate, retail trade); different stacking methods (Mixture of Experts)

3)Time-series experiment preparation: haven’t done time-series predictions before ---> use R (more packages widely used) ---> turn different date format into standard forms (including additional data)

4)BSTS: read BSTS package description and tutorials on line ---> make comparison between plain model and plus one ---> no improvement ---> try more additional data (initial claims, leading index, total non-farm employment...) ---> no differences

5)Mixture of Experts: No idea and few tutorials ---> read papers (so difficult! Couldn’t understand the algorithms) ---> procrastination & depressed ---> read old papers ---> Mixture of Experts seldom used in time-series predictions and may unsuitable ---> give up

*Reread the paper and focus on the methods part---> decide to replay these methods since trying new methods is more difficult than I think ---> explore the principals authors used to evaluate models*

6)LSTM: read LSTM theories (based on RNN; differences from RNN) ---> read tutorials & python package descriptions ---> try one tutorial (normal time-series prediction preprocessing: supervised learning, stationary, scaled) ---> couldn’t understand the parameter ---> try another tutorial ---> come out with the outcomes

7)Classical methods(python) : ARIMA read tutorials and try ---> ETS ---> SVR

8)Replay BSTS and find ways to get the MAE, RMSE

