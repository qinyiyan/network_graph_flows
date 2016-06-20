#####preparing ingredients :) #######

l = []
with open("/Users/qinyiyan/Google Drive/EE232E/proj2/project_2_data/movie_genre.txt",'r') as in_file:
    for line in in_file:
        line = line.strip()
        #print len(line)
        if len(line) > 0:
          l.append(map(str, line.split('\t\t')))

import re
def filter_parenthesis(movie):
    new = re.sub(r'\s*\(\D*\).*',"", movie)
    return new


for each in l:
    each[0] = filter_parenthesis(each[0])

movies = set()
for each in l:
    movies.update([each[0]])

ll = []
with open("/Users/qinyiyan/Google Drive/EE232E/proj2/project_2_data/movie_rating.txt",'r') as in_file:
    for line in in_file:
        line = line.strip()
        #print len(line)
        if len(line) > 0:
            ll.append(map(str, line.split('\t\t')))
s = sorted(ll, key = lambda x: x[1],reverse = True)

for each in ll:
    each[0] = filter_parenthesis(each[0])

for each in s:
    movies.update([each[0]])

movie2index_hash = {}
count = 0
for each in movies:
    movie2index_hash[each] = str(count+1)
    count = count+1

genre2movie_hash = {}#store movie in index format
movie2genre_hash = {}
for each in l:
    movieIndex = movie2index_hash[each[0]]
    movie2genre_hash[movieIndex]= each[1]
    if each[1] not in genre2movie_hash:
        genre2movie_hash[each[1]] = []
        genre2movie_hash[each[1]].append(movieIndex)

movie2index2rating = []
for i in range(0, len(s)):
    movie_index = []
    movie_index.append(s[i][0])
    movie_index.append(movie2index_hash[s[i][0]])#从1开始编号
    movie_index.append(s[i][1])
    movie2index2rating.append(movie_index)

movieindex2rating_hash = {}
for i in range(0, len(s)):
    movieindex2rating_hash[movie2index_hash[s[i][0]]]=s[i][1]

l = []
with open("/Users/qinyiyan/Google Drive/EE232E/proj2/project_2_data/actor_movies.txt",'r') as in_file:
    for line in in_file:
        line = line.strip()
        #print len(line)
        if len(line) > 0:
          l.append(map(str, line.split('\t\t')))
with open("/Users/qinyiyan/Google Drive/EE232E/proj2/project_2_data/actress_movies.txt",'r') as in_file:
    for line in in_file:
        line = line.strip()
        #print len(line)
        if len(line) > 0:
          l.append(map(str, line.split('\t\t')))

for each in l:
    for i in range(1, len(each)):
        each[i] = filter_parenthesis(each[i])

actor2index2movie = []
for i in range(0, len(l)):
    actor_index = []
    actor_index.append(l[i][0])
    actor_index.append(str(i+1))#从1开始编号
    for j in range(1,len(l[i])):
        actor_index.append(l[i][j])
    actor2index2movie.append(actor_index)

actor2index_hash = {}
for each in actor2index2movie:
    actor2index_hash[each[0]] = each[1]

actor2movie_hash = {}
for each in actor2index2movie:
    actor2movie_hash[each[1]] = []
    if len(each)>2:
        for j in range(2,len(each)):
            if each[j] in movie2index_hash:
                actor2movie_hash[each[1]].append(movie2index_hash[each[j]])

movie2actors_hash = {}#all index in str format
print len(actor2index2movie)
print len(movie2actors_hash)
for i in range(0,len(actor2index2movie)):
    if len(actor2index2movie[i])>2:
        for j in range(2,len(actor2index2movie[i])):
            if actor2index2movie[i][j] in movie2index_hash:
                #print "a"
                if movie2index_hash[actor2index2movie[i][j]] not in movie2actors_hash:
                    #print "c"
                    movie2actors_hash[movie2index_hash[actor2index2movie[i][j]]] = set()
                    movie2actors_hash[movie2index_hash[actor2index2movie[i][j]]].update([actor2index2movie[i][1]])
                else:
                    #print " b"
                    movie2actors_hash[movie2index_hash[actor2index2movie[i][j]]].update([actor2index2movie[i][1]])
print  len(movie2actors_hash)
#print movie2actors_hash.keys()

index1 = movie2index_hash["Batman v Superman: Dawn of Justice (2016)"]
index2 = movie2index_hash["Mission: Impossible - Rogue Nation (2015)"]
index3 = movie2index_hash["Minions (2015)"]
index = []
index.append(index1)
index.append(index2)
index.append(index3)

print len(movie2actors_hash)
#filter the ones with <5 actors#
for movie in movie2actors_hash.keys():
    if len(movie2actors_hash[movie])<5:
        del movie2actors_hash[movie]
print len(movie2actors_hash)


############## process to cook everything together ###########
def compute_jaccard_index(set_1, set_2):
    n = len(set_1.intersection(set_2))
    return n / float(len(set_1) + len(set_2) - n) 

movie_graph = []
for movie in movie2actors_hash.keys():
    for movie2 in movie2actors_hash.keys():
        if  movie2 != movie:
            J = compute_jaccard_index(movie2actors_hash[movie],movie2actors_hash[movie2])
            if J>0:
                movie_graph.append(movie)
                movie_graph.append(movie2)
                movie_graph.append(J)
    del movie2actors_hash[movie]

f = open("/Users/qinyiyan/Google Drive/EE232E/proj2/project_2_data/movie_graph_notebook.txt",'w')
count = 0
for element in movie_graph:
    if count == 2:
        f.write(str(element))
        f.write("\n")
        count =0
    else:
        f.write(str(element))
        f.write("\t")
        count = count+1

f = open("/Users/qinyiyan/Google Drive/EE232E/proj2/project_2_data/movie2index.txt",'w')
for key in movie2index_hash:
    f.write(str(key))
    f.write("\t")
    f.write(str(movie2index_hash[key]))
    f.write("\n")


count = 0
movie_graph2 = []
edge = []
for element in movie_graph:
    if count == 0:
        edge = []
        edge.append(element)
        count = count +1
    elif count == 2:
        edge.append(element)
        movie_graph2.append(edge)
        count =0
    else:
        edge.append(element)
        count = count+1


##these two are basically the same

movie_graph2 = []
with open("/Users/qinyiyan/Google Drive/EE232E/proj2/project_2_data/movie_graph_notebook.txt",'r') as in_file:
    for line in in_file:
        line = line.strip()
        #print len(line)
        if len(line) > 0:
            movie_graph2.append(map(str, line.split('\t')))


vertex = set()
for each in movie_graph2:
    vertex.update([each[0]]) 
    vertex.update([each[1]])
print len(vertex)

f = open("/Users/qinyiyan/Google Drive/EE232E/proj2/project_2_data/vertex_movie_index.txt",'w')
for each in vertex:
    f.write(each)
    f.write("\n")


#############  part 7 & part 8 ##################

movie2director_hash = {}
director2movie_hash = {}

with open("/Users/qinyiyan/Google Drive/EE232E/proj2/project_2_data/director_movies.txt",'r') as movie_director_file:
    for line in movie_director_file:
        line = line.strip()
        #print len(line)
        if len(line) > 0:
            temp= map(str, line.split('\t\t'))
#             print temp[0]
#             print "haha"
#             print temp[1]
            director2movie_hash[temp[0]] =[]
            if len(temp) >1:
                for i in range(1,len(temp)):
                    director2movie_hash[temp[0]].append(temp[i])
                    if temp[i] not in movie2director_hash:
                        movie2director_hash[temp[i]]=[]
                        movie2director_hash[temp[i]].append(temp[0])
                    else:
                        movie2director_hash[temp[i]].append(temp[0])
        

rank = 0
count = 0
top_100_director_hash = {}
while rank<100:
    if s[count][0] in movie2director_hash:
        for director in range(0,len(s[movie2index2rating[count][0]])):
            if movie2director_hash[s[count][]][director] not in top_100_director_hash:
                top_100_director_hash[s[movie2index2rating[count][0]][director]] = []
                top_100_director_hash[s[movie2index2rating[count][0]][director]].append(s[count][1])
                rank = rank +1
            else:
                top_100_director_hash[movie2director_hash[movie2index2rating[count][1]][director]].append(movie2index2rating[count][1])
        
        count = count+1
    else: 
        count = count+1

##these two are the same

top_100_director_hash = {}
rank =1
with open("/Users/qinyiyan/Google Drive/EE232E/proj2/project_2_data/movie_graph_notebook.txt",'r') as in_file:
    for line in in_file:
        line = line.strip()
        #print len(line)
        if len(line) > 0:
            top_100_director_hash[rank] = line

top_directors = top_100_director_hash.keys()

top_100_director_hash = {}
f = open("/Users/qinyiyan/Google Drive/EE232E/proj2/project_2_data/director_movies.txt",'w')
for each in top_directors:
    #print each
    f.write(str(each))
    f.write("\n")

def list_rating_mean(list):
    count = 0
    rating = 0
    for each in list:
        if each in movieindex2rating_hash:
            count = count+1
            rating = rating + float (movieindex2rating_hash[each])
    rating = float(rating)/float(count)
    return rating

############ importing some files from R to construct feature array #######

f = open("/Users/qinyiyan/Google Drive/EE232E/proj2/project_2_data/actorIndex.txt",'w')
for each in actor2index_hash.keys():
    f.write(each)
    f.write("\t")
    f.write(actor2index_hash[each])
    f.write("\n")


actor_graph = []
for each in l:
    if each[0] in actor2index_hash and each[1] in actor2index_hash:
        edge = []
        edge.append(actor2index_hash[each[0]])
        edge.append(actor2index_hash[each[1]])
        edge.append(each[2])
        actor_graph.append(edge)


actor_vertex = set()
for each in actor_graph:
    actor_vertex.update([each[0]])
    actor_vertex.update([each[1]])


f1 = open("/Users/qinyiyan/Google Drive/EE232E/proj2/project_2_data/actor_index_vertex.txt",'w')
f2 = open("/Users/qinyiyan/Google Drive/EE232E/proj2/project_2_data/actor_graph.txt",'w')
for each in actor_graph:
    f2.write(each[0])
    f2.write("\t")
    f2.write(each[1])
    f2.write("\t")
    f2.write(each[2])
    f2.write("\n")
    
for each in actor_vertex:
    f1.write(each)
    f1.write("\n")

f = open("/Users/qinyiyan/Google Drive/EE232E/proj2/project_2_data/movie2rating.txt",'w')
for each in movieindex2rating_hash:
    f.write(each)
    f.write("\t")
    f.write(movieindex2rating_hash[each])
    f.write("\n")


l=[]
with open ("/Users/qinyiyan/Google Drive/EE232E/proj2/project_2_data/pagerank.txt",'r')as pagerank_file:
    for line in pagerank_file:
        line = line.strip()
        if len(line) > 0:
            l.append(map(str, line.split('\t')))

def add_space(actor):
    new = re.sub(r'\,',", ", actor)
    new = re.sub(r'\(', " (", new)
    new = re.sub(r'([a-z])([A-Z])',"\\1 \\2",new)
    new = re.sub(r'\.(\w)',". \\1",new)
    new = re.sub(r'([A-Z])\. ([A-Z])(\W)',"\\1.\\2\\3",new)
    new = re.sub(r'(\w)\'(\w)', "\\1 \'\\2",new)
    new = re.sub(r'(d|D|O) \'(\w)',"\\1\'\\2",new)
    new = re.sub(r'(Au|Di|Mc|Mac|La|Le)\s([A-Z])',"\\1\\2",new)
    
    return new

actor2pagerank_hash = {}
for each in l:
    each[0] = add_space(each[0])
    if each[0] in actor2index_hash:
        actor2pagerank_hash[actor2index_hash[each[0]]] = each[1]
    else:
        print "no actor in index hash"
        print each[0]


############ calculating community values ########
l=[]
with open ("/Users/qinyiyan/Google Drive/EE232E/proj2/project_2_data/R_movie2fc_rating.txt",'r')as movie2fc_rating:
    for line in movie2fc_rating:
        line = line.strip()
        if len(line) > 0:
            l.append(line)
count = 0
movie2fcrating_hash = {}
for i in range(0,len(l)/2):
    movie2fcrating_hash[l[count]] =l[count+1]
    count = count+2


l=[]
with open ("/Users/qinyiyan/Google Drive/EE232E/proj2/project_2_data/R_movie2neighbor_rating.txt",'r')as R_movie2neighbor_rating:
    for line in R_movie2neighbor_rating:
        line = line.strip()
        if len(line) > 0:
            l.append(line)
count = 0
movie2neighborrating_hash = {}
for i in range(0,len(l)/2):
    movie2neighborrating_hash[l[count]] =l[count+1]
    count = count+2


print len(movie2fcrating_hash)
for each in movie2fcrating_hash.keys():
    if movie2fcrating_hash[each] == 'NaN':
        del movie2fcrating_hash[each]
print len(movie2fcrating_hash)
​
print len(movie2neighborrating_hash)
for each in movie2neighborrating_hash.keys():
    if movie2neighborrating_hash[each] == 'NaN':
        del movie2neighborrating_hash[each]
print len(movie2neighborrating_hash)
​

########### constructing features #############
def is_famous_director (movie):
    if movie in movie2director_hash:
        if movie2director_hash[movie] in top_100_director_hash:
            return top_100_director_hash[director]
        else:
            return 0
    else:
        return 0
    

def genre_rating (genre):
    score = 0
    count = 0
    for each in genre2movie_hash[genre]:
        if each in movieindex2rating_hash:
            score = score + float(movieindex2rating_hash[each])
            count = count +1
    if count>0:
        score = float(score)/float(count)
    return score


def make_feature(top5_pagerank, is_famous_director, neighbor_rating,fc_rating, genre_rating):
    feature = []
    for each in top5_pagerank:
        feature.append(float(each))
    feature.append(is_famous_director)
    feature.append(float(neighbor_rating))
    feature.append(float(fc_rating))
    feature.append(genre_rating)
    return feature

def get_feature(movie):
    top5_pagerank = []
    for each in movie2actors_hash[movie]:
        if each in actor2pagerank_hash:
            top5_pagerank.append(float(actor2pagerank_hash[each]))
    if len(top5_pagerank) == 0:
        return 0
    if len(top5_pagerank)>5:
        top5_pagerank = sorted(top5_pagerank, reverse=True)[:5]
    if len(top5_pagerank)<5:
        top5_pagerank = sorted(top5_pagerank, reverse=True)
        for i in range(len(top5_pagerank), 5):
            top5_pagerank.append(sum(top5_pagerank)/float(len(top5_pagerank)))
    director = is_famous_director(movie)
    neighbor_rating = movie2neighborrating_hash[movie]
    fc_rating = movie2fcrating_hash[movie]
    genre_rate = genre_rating(movie2genre_hash[movie])
    return make_feature(top5_pagerank, director, neighbor_rating, fc_rating, genre_rate)


############## now, time to regression! STIR AND FRY.....##########
X = []
y = []

for each in movie2neighborrating_hash:
    if each in movie2fcrating_hash and each in movie2genre_hash and each in movieindex2rating_hash:
        feature = get_feature(each)
        if feature !=0:
            X.append(feature)
            y.append(float(movieindex2rating_hash[each]))

import numpy
X = numpy.array(X)
y = numpy.array(y)


##########linear regression ######
feature1 =  get_feature('458218')#mission
feature2 = get_feature('318106')#minion
feature3 = get_feature('880573')#batman
X_predict = []
X_predict.append(feature1)
X_predict.append(feature2)
X_predict.append(feature3)
X_predict = numpy.array(X_predict)

from sklearn.linear_model import LinearRegression
#X = []  # put your dates in here
#y = []  # put your kwh in here
model = LinearRegression()
model.fit(X, y)

#X_predict = []  # put the dates of which you want to predict kwh here
y_predict = model.predict(X_predict)
print y_predict

#######polinomial regression #######
from sklearn.preprocessing import PolynomialFeatures
from sklearn import linear_model

X_ = X 
vector = y
predict= X_predict

poly = PolynomialFeatures(degree=2)
X_ = poly.fit_transform(X)
predict_ = poly.fit_transform(predict)

clf = linear_model.LinearRegression()
clf.fit(X_, vector)
print clf.predict(predict_)


######SVM regression##########
from sklearn.svm import SVR
import matplotlib.pyplot as plt

###############################################################################
# Fit regression model
svr_rbf = SVR(kernel='rbf', C=1e3, gamma=0.1)
svr_lin = SVR(kernel='linear', C=1e3)
svr_poly = SVR(kernel='poly', C=1e3, degree=2)
y_rbf = svr_rbf.fit(X, y).predict(X_predict)
y_lin = svr_lin.fit(X, y).predict(X_predict)
y_poly = svr_poly.fit(X, y).predict(X_predict)

print y_rbf
print y_lin
print y_poly

########plot! make the dish look even better!#######
# look at the results
plt.scatter(X, y, c='k', label='data')
plt.hold('on')
plt.plot(X, y_rbf, c='g', label='RBF model')
plt.plot(X, y_lin, c='r', label='Linear model')
plt.plot(X, y_poly, c='b', label='Polynomial model')
plt.xlabel('data')
plt.ylabel('target')
plt.title('Support Vector Regression')
plt.legend()
plt.show()



#########alright, calculate error: don't forget the calories #########
count = 0
X_predict = []
Y_expect = []
while count <100:
    X_predict.append(X[count])
    Y_expect.append([y[count]])
    count = count +1
X_predict = numpy.array(X_predict)


#linear regression: 
y_predict = model.predict(X_predict)
#print y_predict
error_rate = 0
for i in range(0, len(y_predict)):
    error_rate = abs(y_predict[i] - Y_expect[i])/Y_expect[i] + error_rate
error_rate = error_rate/len(y_predict)
print error_rate

#polynomial regression:

predict_X = poly.fit_transform(X_predict)

y_predict = clf.predict(predict_X)
error_rate = 0
for i in range(0, len(y_predict)):
    error_rate = abs(y_predict[i] - Y_expect[i])/Y_expect[i] + error_rate
error_rate = error_rate/len(y_predict)
print error_rate


# SVM linear regression

y_lin_model = svr_lin.fit(X, y)
y_predict = y_lin_model.predict(X_predict)
error_rate = 0
for i in range(0, len(y_predict)):
    error_rate = abs(y_predict[i] - Y_expect[i])/Y_expect[i] + error_rate
error_rate = error_rate/len(y_predict)
print error_rate

#SVM rbf regression

y_rbf_model = svr_rbf.fit(X, y)
y_predict = y_rbf_model.predict(X_predict)
error_rate = 0
for i in range(0, len(y_predict)):
    error_rate = abs(y_predict[i] - Y_expect[i])/Y_expect[i] + error_rate
error_rate = error_rate/len(y_predict)
print error_rate


#SVM rbf regression

y_poly_model = svr_poly.fit(X, y)
y_predict = y_poly_model.predict(X_predict)
error_rate = 0
for i in range(0, len(y_predict)):
    error_rate = abs(y_predict[i] - Y_expect[i])/Y_expect[i] + error_rate
error_rate = error_rate/len(y_predict)
print error_rate


#########python cook book: IMDb Database Exploration #########
######################Finale#######################
