import sqlite3
import json

cnx = sqlite3.connect('vectors.db')
cnx.text_factory = str

cur = cnx.cursor()

cur.execute('DROP TABLE IF EXISTS child_books;')
cur.execute('DROP TABLE IF EXISTS childes_both;')
cur.execute('DROP TABLE IF EXISTS childes_parents;')
cur.execute('DROP TABLE IF EXISTS childes_children;')
cur.execute('DROP TABLE IF EXISTS adult_speech;')
cur.execute('DROP TABLE IF EXISTS gutenberg;')
cur.execute('DROP TABLE IF EXISTS pbs_kids;')
cur.execute('DROP TABLE IF EXISTS pbs_adults;')
cur.execute('DROP TABLE IF EXISTS nickelodeon;')
cur.execute('DROP TABLE IF EXISTS disney;')
cur.execute('DROP TABLE IF EXISTS kids_tv_combined;')
cur.execute('DROP TABLE IF EXISTS simply_scripts;')

cur.execute('CREATE TABLE child_books (word VARCHAR(255), vector TEXT, PRIMARY KEY (word));')
cur.execute('CREATE TABLE childes_both (word VARCHAR(255), vector TEXT, PRIMARY KEY (word));')
cur.execute('CREATE TABLE childes_children (word VARCHAR(255), vector TEXT, PRIMARY KEY (word));')
cur.execute('CREATE TABLE childes_parents (word VARCHAR(255), vector TEXT, PRIMARY KEY (word));')
cur.execute('CREATE TABLE adult_speech (word VARCHAR(255), vector TEXT, PRIMARY KEY (word));')
cur.execute('CREATE TABLE gutenberg (word VARCHAR(255), vector TEXT, PRIMARY KEY (word));')
cur.execute('CREATE TABLE pbs_kids (word VARCHAR(255), vector TEXT, PRIMARY KEY (word));')
cur.execute('CREATE TABLE nickelodeon (word VARCHAR(255), vector TEXT, PRIMARY KEY (word));')
cur.execute('CREATE TABLE disney (word VARCHAR(255), vector TEXT, PRIMARY KEY (word));')
cur.execute('CREATE TABLE kids_tv_combined (word VARCHAR(255), vector TEXT, PRIMARY KEY (word));')
cur.execute('CREATE TABLE pbs_adults (word VARCHAR(255), vector TEXT, PRIMARY KEY (word));')
cur.execute('CREATE TABLE simply_scripts (word VARCHAR(255), vector TEXT, PRIMARY KEY (word));')

tablenames = ["child_books", "childes_both", "childes_children", "childes_parents", "adult_speech", "gutenberg", "pbs_kids", "nickelodeon", "disney", "kids_tv_combined", "pbs_adults", "simply_scripts"]
filenames = ["models_child_books.txt", "models_childes_both.txt", "models_childes_children.txt", "models_childes_parents.txt", "models_adult_speech.txt", "models_gutenberg.txt", "models_pbs_kids.txt", "models_nickelodeon.txt", "models_disney.txt", "models_kids_tv_combined.txt", "models_pbs_adults.txt", "models_simply_scripts.txt"]

for tablename, filename in zip(tablenames, filenames):
    with open(filename, 'r') as s:
        jobs = 0
        for line in s:
            row = line.rstrip().split(' ')
            word = str(row[0])
            if len(word) <= 255:
                vector = [float(val) for val in row[1:]]
                s_vector = json.dumps(vector)
                sql = 'INSERT INTO {} (word, vector) VALUES (?, ?)'.format(
                    tablename)
                insert_data = (word, s_vector)
                cur.execute(sql, insert_data)
                jobs += 1
                if jobs % 1000000 == 0:
                    print('Jobs done: {0}'.format(jobs))
            else:
                print('one exclusion: ', word)
    cnx.commit()

    cur.execute('SELECT count(word), count(vector) FROM {};'.format(tablename))
    print(cur.fetchall())

cnx.close()
