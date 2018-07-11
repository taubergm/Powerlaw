All code and data used for medium post https://medium.com/@michaeltauberg/power-law-in-popular-media-7d7efef3fb7c


Files
-----

1) get_hits.py
  -> Uses https://github.com/guoguo12/billboard-charts  python lib
  -> scrapes billboard hot-100 songs for a set number of days from present (specified in num_days_to_get var)
  
2) get_fiction_sellers.py
  -> uses NYT api to get a list of the fiction bestsellers 
  
3) get_movie_data.py
  -> Uses a combination of python wbtools lib and beautiful soup to get wikipedia data on hollywood movies4)
  
4) Powerlaw.R
  -> reads the data csvs (generated from python or found on the web) and creates ggplot-based visualization
