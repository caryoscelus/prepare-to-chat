#! /usr/bin/env sh

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

BASE=bower_components/wikiquote-fortune-collection/archive
FILES=`ls ${BASE} | grep -v README`
TARGET=html/quotes.js

echo "will try to jsonify following files:"
echo "${FILES}"
echo "will write into ${TARGET}"

for f in ${FILES}; do
    # remove "special lines"
    cat ${BASE}/${f} | grep -v "^$" | grep -v "^%" | grep -v "^\s" > ${f}
done

cat ${FILES} | ./jsonify.py > html/quotes.js
rm ${FILES}
