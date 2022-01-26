import sqlite3
import json
import random
import numpy as np
from scipy import spatial
from itertools import combinations
import pandas as pd
from math import sqrt
from collections import OrderedDict

cnx = sqlite3.connect('vectors.db')
cnx.text_factory = str
np.random.seed(111)

cur = cnx.cursor()

memorybank = {}

def weighted_std(values, weights):
	"""
	Return the weighted standard deviation.

	values, weights -- Numpy ndarrays with the same shape.
	"""
	average = np.average(values, weights=weights)
	# Fast and numerically precise:
	variance = np.average((values-average)**2, weights=weights)
	# Small sample size bias correction:
	variance_ddof1 = variance*len(values)/(len(values)-1)
	return sqrt(variance_ddof1)

def within_group_cohesion(X):
	dist = spatial.distance.pdist(X, 'cosine')
	return dist.mean()

def group_cohesion_test(X, Y, perm_n = 1000, permtype = 1):

	test_statistic = np.average((within_group_cohesion(X), within_group_cohesion(Y)),
		weights = (len(X), len(Y)))
	jointlist = np.concatenate((X,Y))
	permutations = np.array([])
	if permtype == 1:
		count = 0
		cutpoint = len(X)
		while count < perm_n:
			np.random.shuffle(jointlist)
			set1 = jointlist[:cutpoint]
			set2 = jointlist[cutpoint:]

			permutations = np.append(permutations, 
				np.average([within_group_cohesion(set1), within_group_cohesion(set2)], 
					weights = [len(set1), len(set2)])
			)
			count += 1
	else:
		nums = list(range(len(jointlist)))
		for comb in combinations(nums, len(X)):
			set1 = [item for i, item in enumerate(jointlist) if i in comb]
			set2 = [item for i, item in enumerate(jointlist) if i not in comb]

			permutations = np.append(permutations, 
				np.average([within_group_cohesion(set1), within_group_cohesion(set2)], 
					weights = [len(set1), len(set2)])
			)
	P_val = (sum(i <= test_statistic for i in permutations)+1)/(len(permutations)+1)
	return P_val


def s_weat(X, A, B, permt = 0, perm_n = 10000, table = "child_books", cohesion_test = False, 
cohesion_permutations = 1000, cohesion_type = 2):

	def diff_sim(X, A, B):

		sum_A = 0
		sum_B = 0

		all_sims = []
		for a in A:
			a_ = a.reshape(1, -1)
			results = spatial.distance.cdist(a_, X, 'cosine')
			sum_X = (1 - results).sum()
			val = sum_X/len(X)
			sum_A += val
			all_sims.append(val)
		ave_A = sum_A/len(A)

		for b in B:
			b_ = b.reshape(1, -1)
			results = spatial.distance.cdist(b_, X, 'cosine')
			sum_X = (1 - results).sum()
			val = sum_X/len(X)
			sum_B += val
			all_sims.append(val)
		ave_B = sum_B/len(B)

		difference = ave_A - ave_B

		# For SD calculation, assign weights based on frequency of opposite category
		weights = [len(B) for num in range(len(A))] + [len(A) for num in range(len(B))]
		standard_dev = weighted_std(all_sims, weights)
		effect_size = difference/standard_dev

		return effect_size

	def permutation_test(X, A, B):
		jointlist = np.array(list(A) + list(B))
		permutations = []
		nums = list(range(len(jointlist)))
		for comb in combinations(nums, len(A)):
			set1 = [item for i, item in enumerate(jointlist) if i in comb]
			set2 = [item for i, item in enumerate(jointlist) if i not in comb]
			permutations.append(diff_sim(X, set1, set2))
		return permutations

	def rand_test(X, A, B, perm_n):
		jointlist = np.array(list(A) + list(B))
		np.random.shuffle(jointlist)
		permutations = []
		count = 0
		cutpoint = len(A)
		while count < perm_n:
			np.random.shuffle(jointlist)
			set1 = jointlist[:cutpoint]
			set2 = jointlist[cutpoint:]
			permutations.append(diff_sim(X, set1, set2))
			count += 1
		return permutations

	allwords = list(X + A + B)
	if table not in memorybank.keys():
		memorybank[table] = {}
	cache = [i for i in allwords if i in memorybank[table].keys()]
	nocache = [i for i in allwords if i not in memorybank[table].keys()]

	if len(nocache) > 0:
		format_strings = ','.join(['?'] * len(nocache))
		cur.execute('SELECT word, vector FROM {0} WHERE word IN ({1})'.format(table, format_strings), tuple(nocache))
		Results = [(x[0], json.loads(x[1])) for x in cur.fetchall()]
		for item in Results:
			memorybank[table][item[0]] = (item[0], item[1])
	else:
		Results = []
	if len(cache) > 0:
		for item in cache:
			Results.append(memorybank[table][item])
	Cat1 = np.array([word[1] for word in Results if word[0] in X])
	Att1 = np.array([word[1] for word in Results if word[0] in A])
	Att2 = np.array([word[1] for word in Results if word[0] in B])

	effect_size = diff_sim(Cat1, Att1, Att2)

	if permt == 1 or permt == 2:
		if permt == 1:
			permutations = np.array(permutation_test(Cat1, Att1, Att2))
		elif permt == 2:
			permutations = np.array(rand_test(Cat1, Att1, Att2, perm_n = perm_n))
		perm_mean = np.mean(permutations)
		permutations = permutations - perm_mean
		sum_c = effect_size - perm_mean
		Pleft = (sum(i <= sum_c for i in permutations)+1)/(len(permutations)+1)
		Pright = (sum(i >= sum_c for i in permutations)+1)/(len(permutations)+1)
		Ptot = (sum(abs(i) >= abs(sum_c) for i in permutations)+1)/(len(permutations)+1)
		se = np.std(permutations)

	if cohesion_test == True:
		cohesion_categories = group_cohesion_test(Att1, Att2, perm_n = cohesion_permutations, permtype = cohesion_type)

	missing = [word for word in allwords if word not in [res[0] for res in Results]]
	result_dict = OrderedDict({
		"table": table,
		# the category labels are not fed to the function, so assign them at higher level upon return
		"categories": None,
		"attribute(s)": None,
		"effect_size": effect_size,
		"missing": missing
	})
	if permt == 1 or permt == 2:
		result_dict["Pleft"] = Pleft
		result_dict["Pright"] = Pright
		result_dict["Ptot"] = Ptot
		result_dict["se"] = se

	if cohesion_test == True:
		result_dict["cohesion_categories"] = cohesion_categories

	return result_dict

if __name__ == '__main__':

	vecs = {
		"female": "she her mommy mom girl mother lady sister mama momma grandma herself".split(),
		"male": "he his daddy dad boy father guy brother dada papa bro grandpa himself".split(), 
	}


	results = []

	tablenames = ["simply_scripts", "pbs_kids", "nickelodeon", "disney", "kids_tv_combined", "pbs_adults", "gutenberg", "childes_parents", "childes_children", "childes_both", "child_books", "childes_young", "childes_old", "adult_speech"]

	for table in tablenames:
		with open("proflist.txt", 'r') as f:
			for line in f:
				attribute = str(line.rstrip())
				try:
					result_dict = s_weat([attribute], vecs["male"], vecs["female"], 
						permt=2, perm_n=1000, table = table, cohesion_test = True, cohesion_permutations = 1000, cohesion_type = 1)
					result_dict["categories"] = "male vs. female"
					result_dict["attribute(s)"] = attribute
					results.append(result_dict)
				except:
					pass

	df = pd.DataFrame.from_dict(results)
	df.to_csv("embeddings_singleprofs_071220.csv", index = False)

cnx.close()
