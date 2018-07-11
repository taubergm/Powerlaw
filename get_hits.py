#!/usr/bin/python
import billboard
import json
import csv
import datetime


BILLBOARD_CHART = 'hot-100'

songs = []
num_days_to_get = 6900
output_csv = "test.csv"

for i in range(0,num_days_to_get):
    #day = datetime.datetime.now() - datetime.timedelta(days=21008) - datetime.timedelta(days=i) 
    day = datetime.datetime.now() - datetime.timedelta(days=1) - datetime.timedelta(days=i) 
    date = str(day.date())
    print date
    

    chart = billboard.ChartData(BILLBOARD_CHART, date)

    # sometimes we get non-ascii characters from the beautiful soup scrape - try this

    with open(output_csv, 'a') as csv_file:
        csv_writer = csv.writer(csv_file)
        csv_writer.writerow(["date", "title", "artist", "peakPos", "lastPos", "weeks", "rank", "change", "spotifyLink", "spotifyID", "videoLink"])
        for song in chart:
            song.title = song.title.encode('utf-8').strip()
            song.artist = song.artist.encode('utf-8').strip()
            song.peakPos = song.peakPos
            song.lastPos = song.lastPos
            song.weeks = song.weeks
            song.rank = song.rank
            song.change = song.change
            song.spotifyLink = song.spotifyLink.encode('utf-8').strip()
            song.spotifyID = song.spotifyID.encode('utf-8').strip()
            song.videoLink = song.videoLink.encode('utf-8').strip()

            csv_writer.writerow([date, song.title, song.artist, song.peakPos, song.lastPos, song.weeks, song.rank, song.change, song.spotifyLink, song.spotifyID, song.videoLink])
    

