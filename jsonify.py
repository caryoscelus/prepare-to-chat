#! /usr/bin/env python3

##
##  Copyright (C) 2014 caryoscelus
##  
##  This program is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##  
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##  
##  You should have received a copy of the GNU General Public License
##  along with this program.  If not, see <http://www.gnu.org/licenses/>.
##

from sys import stdin, stdout
import json

lines = stdin.readlines()
quotes = {}

for line in lines:
    spl = line.split(': ')
    if len(spl) < 2:
        continue
    author = spl[0]
    quote = ': '.join(spl[1:])
    qs = [q for q in quote.split('\n') if q]
    if not author in quotes:
        quotes[author] = []
    quotes[author] += qs

# remove authors with <10 quotes
quotes = { k : quotes[k] for k in quotes if len(quotes[k]) >= 10 }

stdout.write('var _allQuotes = ')
stdout.write(json.dumps(quotes))
