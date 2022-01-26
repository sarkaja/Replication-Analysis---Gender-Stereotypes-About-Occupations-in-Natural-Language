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

def weat(X, Y, A, B, permt = 0, perm_n = 10000, table = "child_books", cohesion_test = False, 
cohesion_permutations = 1000, cohesion_type = 2):

	def diff_sim(X, Y, A, B):

		sum_X = 0
		for x in X:
			sum_X += sim(x, A, B)

		sum_Y = 0
		for y in Y:
			sum_Y += sim(y, A, B)

		difference = sum_X/len(X) - sum_Y/len(Y)

		all_sims = []
		for w in (np.concatenate((X,Y))):
			all_sims.append(sim(w, A, B))
		# For SD calculation, assign weights based on frequency of opposite category
		weights = [len(Y) for num in range(len(X))] + [len(X) for num in range(len(Y))]
		standard_dev = weighted_std(all_sims, weights)
		effect_size = difference/standard_dev
			
		return effect_size

	def sim(w, A, B):

		w_ = w.reshape(1, -1)

		results = spatial.distance.cdist(w_, A, 'cosine')
		sum_A = (1 - results).sum()

		results = spatial.distance.cdist(w_, B, 'cosine')
		sum_B = (1 - results).sum()
		
		difference = sum_A/len(A) - sum_B/len(B)
		
		return difference

	def permutation_test(X, Y, A, B):
		jointlist = np.array(list(X) + list(Y))
		permutations = []
		nums = list(range(len(jointlist)))
		for comb in combinations(nums, len(X)):
			set1 = [item for i, item in enumerate(jointlist) if i in comb]
			set2 = [item for i, item in enumerate(jointlist) if i not in comb]
			permutations.append(diff_sim(set1, set2, A, B))
		return permutations

	def rand_test(X, Y, A, B, perm_n):
		jointlist = np.array(list(X) + list(Y))
		np.random.shuffle(jointlist)
		permutations = []
		count = 0
		cutpoint = len(X)
		while count < perm_n:
			np.random.shuffle(jointlist)
			set1 = jointlist[:cutpoint]
			set2 = jointlist[cutpoint:]
			permutations.append(diff_sim(set1, set2, A, B))
			count += 1
		return permutations

	allwords = list(X + Y + A + B)
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
	Cat2 = np.array([word[1] for word in Results if word[0] in Y])
	Att1 = np.array([word[1] for word in Results if word[0] in A])
	Att2 = np.array([word[1] for word in Results if word[0] in B])

	effect_size = diff_sim(Cat1, Cat2, Att1, Att2)

	if permt == 1 or permt == 2:
		if permt == 1:
			permutations = np.array(permutation_test(Cat1, Cat2, Att1, Att2))
		elif permt == 2:
			permutations = np.array(rand_test(Cat1, Cat2, Att1, Att2, perm_n = perm_n))
		perm_mean = np.mean(permutations)
		permutations = permutations - perm_mean
		sum_c = effect_size - perm_mean
		Pleft = (sum(i <= sum_c for i in permutations)+1)/(len(permutations)+1)
		Pright = (sum(i >= sum_c for i in permutations)+1)/(len(permutations)+1)
		Ptot = (sum(abs(i) >= abs(sum_c) for i in permutations)+1)/(len(permutations)+1)
		se = np.std(permutations)
	
	if cohesion_test == True:
		cohesion_categories = group_cohesion_test(Cat1, Cat2, perm_n = cohesion_permutations, permtype = cohesion_type)
		cohesion_attributes = group_cohesion_test(Att1, Att2, perm_n = cohesion_permutations, permtype = cohesion_type)

	missing = [word for word in allwords if word not in [res[0] for res in Results]]
	result_dict = OrderedDict({
		"table": table,
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
		result_dict["cohesion_attributes"] = cohesion_attributes

	return result_dict


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
		"female": "she her mommy mom girl mother lady sister mama momma sis grandma herself".split(),
		"male": "he his daddy dad boy father guy brother dada papa bro grandpa himself".split(), 
		"femalenew": "hers women woman daughter wife gal female girlfriend queen grandmother aunt granddaughter niece gentlewoman sorority bachelorette princess dame gentlewomen stepmother ms madam bride stepdaughter maiden godmother grandma missus heroine motherhood sororities ma".split(),
		"malenew": "him man men son husband guy male boyfriend king grandfather uncle grandson nephew lad gentleman fraternity bachelor prince dude gentlemen stepfather sir bloke groom stepson suitor godfather fella hero fatherhood fraternities pa".split(),
		"femalemax": "her hers she women woman herself daughter mother wife gal girl sister female mom girlfriend queen grandmother aunt granddaughter niece lady gentlewoman sorority bachelorette princess dame gentlewomen stepmother mommy ms madam bride stepdaughter maiden godmother grandma missus heroine motherhood sororities mama ma".split(),
		"malemax": "he his him man men himself son father husband guy boy brother male dad boyfriend king grandfather uncle grandson nephew lad gentleman fraternity bachelor prince dude gentlemen stepfather daddy sir bloke groom stepson suitor godfather grandpa fella hero fatherhood fraternities papa pa".split(),
		"home": "baby house home wedding kid family marry".split(), 
		"work": "work office job business trade activity act money".split(), 
		"homemax": "baby house home wedding kid family marry marriage parent caregiving children cousins relative kitchen cook".split(), 
		"workmax": "work office job business trade money manage executive professional corporate corporation salary career hire hiring".split(),
		"reading": "book read write story word writing reading tale".split(), 
		"math": "puzzle number count math counting calculator subtraction addition".split(), 
		"readingmax": "book read write story word writing reading tale novel literature narrative sentence paragraph phrase diary notebook fiction nonfiction".split(), 
		"mathmax": "puzzle number count math counting calculator subtraction addition multiplication division algebra geometry calculus equations computation computer".split(), 
		"arts": "art dance dancing sing singing paint painting song draw drawing".split(), 
		"science": "science scientist chemistry physic engineer space spaceship astronaut chemical microscope".split(), 
		"artsmax": "art dance dancing dancer jig sing singing singer paint painting portrait sketch picture painter song draw drawing poet poetry symphony melody music musician sculpture create artistry designer compose".split(), 
		"sciencemax": "science scientist chemistry chemist physic doctor medicine engineer space spaceship astronaut chemical microscope technology Einstein NASA experiment astronomy data analyze atom molecule lab".split(), 
		"instruments": "guitar banjo drum tuba bell fiddle harpsichord piano flute horn violin".split(), 
		"weapons": "weapon arrow club gun spear sword dynamite rifle tank bomb knife cannon slingshot whip".split(), 
		"good": "good happiness happy fun fantastic lovable magical delight joy relaxing honest excited laughter lover cheerful".split(), 
		"bad": "bad torture murder abuse wreck die disease disaster mourning virus killer nightmare stress kill death".split(),
	}

	s_tests = [
		("male", "female", "home"),
		("male", "female", "reading"),
		("male", "female", "arts"),
		("male", "female", "work"),
		("male", "female", "math"),
		("male", "female", "science"),
		("male", "female", "good"),
		("male", "female", "bad"),
		("instruments", "weapons", "good"),
		("instruments", "weapons", "bad")
	]

	db_tests = [
		("male", "female", "home", "work"),
		("male", "female", "science", "arts"),
		("male", "female", "reading", "math"),
		("male", "female", "good", "bad"),
        ("malenew", "femalenew", "home", "work"),
		("malenew", "femalenew", "science", "arts"),
		("malenew", "femalenew", "reading", "math"),
		("malenew", "femalenew", "good", "bad"),
		("malenew", "femalenew", "homemax", "workmax"),
		("malenew", "femalenew", "sciencemax", "artsmax"),
		("malenew", "femalenew", "readingmax", "mathmax"),
        ("malemax", "femalemax", "home", "work"),
		("malemax", "femalemax", "science", "arts"),
		("malemax", "femalemax", "reading", "math"),
		("malemax", "femalemax", "good", "bad"),
		("malemax", "femalemax", "homemax", "workmax"),
		("malemax", "femalemax", "sciencemax", "artsmax"),
		("malemax", "femalemax", "readingmax", "mathmax"),
		("instruments", "weapons", "good", "bad")
	]

	results = []

	tablenames = ["child_books", "childes_both", "childes_children", "childes_parents", "adult_speech", "gutenberg", "pbs_kids", "nickelodeon", "disney", "kids_tv_combined", "pbs_adults", "simply_scripts"] # same tablenames as from import_embeddings

	for table in tablenames:

		try:
			for (att1, att2, att3) in s_tests:
				result_dict = s_weat(vecs[att3], vecs[att1], vecs[att2], 
					permt=2, perm_n=1000, table = table, cohesion_test = True, cohesion_permutations = 1000, cohesion_type = 1)
				result_dict["categories"] = att1 + " vs. " + att2
				result_dict["attribute(s)"] = att3
				results.append(result_dict)

			for (att1, att2, att3, att4) in db_tests:
				result_dict = weat(vecs[att1], vecs[att2], vecs[att3], vecs[att4],
					permt=2, perm_n=1000, table = table, cohesion_test = True, cohesion_permutations = 1000, cohesion_type = 1)
				result_dict["categories"] = att1 + " vs. " + att2
				result_dict["attribute(s)"] = att3 + " vs. " + att4
				results.append(result_dict)
		except Exception as e:
			print(e)

	df = pd.DataFrame.from_dict(results)
	df.to_csv("embeddings_results_all_071220.csv", index = False)

cnx.close()

