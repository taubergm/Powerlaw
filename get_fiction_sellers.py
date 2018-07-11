import pprint
import requests
import csv
import datetime


#sample request: https://api.nytimes.com/svc/books/v3/lists/2017-03-13/Combined%20Print%20and%20E-Book%20Fiction.json?api_key=<your_api_key>
#BOOKS_ROOT = "https://api.nytimes.com/svc/books/v3/lists/best-sellers"

BOOKS_ROOT = "https://api.nytimes.com/svc/books/v3/lists/"
LIST = "Combined%20Print%20and%20E-Book%20Fiction.json"   #you can change to other lists available at https://developer.nytimes.com
#LIST = "best-sellers/history.json"


# Add your API key here 
#API_KEY = "13d664889d584dbab46fcf8baa54ef20"
#API_KEY = "ee5cdf98ae274b28a1b6060378cba7ab"
API_KEY= "8e1c954a79ff4743852c8d5bcc7f72db"

bestsellers = []
num_days_to_get = 365 # you can use up to 1000 api calls a day so max=1000
offset = 0

outfile = "bestsellers_2018.csv"
with open(outfile, 'wb') as output_file:
            
    keys = ["date", "author", "publisher", "age_group", "description", "primary_isbn10", "primary_isbn13", "title", "article_chapter_link", 
                            "sunday_review_link", "first_chapter_link", "contributor", "price", "bestsellers_date", "dagger", "display_name",
                            "list_name", "published_date", "rank", "rank_last_week", "weeks_on_list", "amazon_product_url"]
                        
    dict_writer = csv.DictWriter(output_file, keys)
    dict_writer.writeheader()
            
    for i in range(0,num_days_to_get):
        day = datetime.datetime.now() - datetime.timedelta(days=i) - datetime.timedelta(days=offset)  
        date = str(day.date())
        print date
        url = "%s/%s/%s?api_key=%s" % (BOOKS_ROOT, date, LIST, API_KEY)
        url = url.strip()  
        print url
        r = requests.get(url)
        if (r.status_code == 200):
            results = r.json()
            books = []
            
            for i in results['results']['books']:
                dic = {}

                #pprint.pprint(i)
                        
                dic['date'] = date
                dic['author'] = ""
                dic['amazon_product_url'] = ""
                dic['article_chapter_link'] = ""
                dic['publisher'] = ""
                dic['age_group'] = ""
                dic['description'] = ""
                dic['primary_isbn10'] = ""
                dic['primary_isbn13'] = ""
                dic['title']= ""
                dic['sunday_review_link'] = ""
                dic['first_chapter_link' ] = ""
                dic['contributor'] = ""
                dic['price']  = ""
                dic['bestsellers_date'] = results['results']['bestsellers_date']
                dic['dagger'] = ""
                dic['display_name'] = results['results']['display_name']
                dic['list_name'] = results['results']['list_name'] 
                dic['published_date'] = results['results']['published_date'] 
                dic['rank'] = ""
                dic['rank_last_week'] = "" 
                dic['weeks_on_list'] = ""

                dic['date'] = date  
                dic['author'] = i['author'].encode("utf8")  
                try:
                    dic['amazon_product_url'] = i['amazon_product_url'].encode("utf8")
                except:
                    print "no amazon_product_url"
                try:
                    dic['publisher'] = i['publisher'].encode("utf8")
                except:
                    print "no publisher"
                try:
                    dic['article_chapter_link'] = i['article_chapter_link'].encode("utf8")
                except:
                    print "no article_chapter_link"
                try:
                    dic['age_group'] = i['age_group'].encode("utf8")
                    dic['description'] = i['description'].encode("utf8")    
                except:
                    print "no age_group/description" 

                try:
                    if (i['isbns'][0] != None):
                        dic['primary_isbn10'] = i['isbns'][0]['isbn10'].encode("utf8")
                        dic['primary_isbn13'] = i['isbns'][0]['isbn13'].encode("utf8")
                except:
                    print "no isbns" 
      
                dic['title'] = i['title'].encode("utf8")
                
                try:
                    dic['sunday_review_link'] = i['sunday_review_link'].encode("utf8")
                except:
                    print "no sunday review link"
                try:
                    #dic['article_chapter_link'] = i['reviews'][0]['article_chapter_link'].encode("utf8")
                    dic['first_chapter_link'] = i['first_chapter_link'].encode("utf8")
                except:
                    print "no first_chapter_link" 
                try:
                    dic['contributor'] = i['contributor'].encode("utf8")
                except:
                    print "no contributor"
                try:
                    dic['price'] = i['price']
                except:
                    print " no price"
                try:
                    dic['dagger'] = i['dagger']
                except:
                    print "no dagger"
                try:
                    dic['rank'] = i['rank']
                except:
                    print "no rank"
                try:
                    dic['rank_last_week']  = i['rank_last_week']
                except:
                    print "no rank_last_week"
                try:
                    dic['weeks_on_list'] = i['weeks_on_list']
                except:
                    print "no weeks_on_list" 
                    #print i['ranks_history']

                books.append(dic)
                #pprint.pprint(dic)

                dict_writer.writerow(dic)
    

    

