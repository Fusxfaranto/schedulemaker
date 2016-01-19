import lxml.html.soupparser

#RESTRICTED_LAB = False;

tree = lxml.html.soupparser.parse("temp.html")

CENTRAL = 0
NORTH = 1
campus_map = {'EECS': NORTH,
              'IOE' : NORTH,
              'CHRYS' : NORTH,
              'DOW' : NORTH,
              'FXB' : NORTH,
              'GGBL' : NORTH,
              'STAMPS' : NORTH,
              'BEYSTER' : NORTH,
              'CHEM' : CENTRAL,
              'MLB' : CENTRAL,
              'DENN' : CENTRAL,
              'WEISER' : CENTRAL,
              'AH' : CENTRAL,
              'CCL' : CENTRAL,
              'MH' : CENTRAL,
              'EH' : CENTRAL,
              'USB' : CENTRAL,
              'RAND' : CENTRAL,
              'EQ' : CENTRAL,
              'TBA' : NORTH}

import copy
def parsetime(s):
    s = s.split(':', 1)
    ampm = s[1][2] == 'P'
    return (10 * (int(s[0]) % 12 + 12 * ampm) + int(s[1][0:2]) / 6) * 10

default_times = {'Mo': [], 'Tu': [], 'We': [], 'Th': [], 'Fr': []}
courses = {}

name = ''.join(tree.xpath('//div[@id="win0divSSR_CLSRSLT_WRK_GROUPBOX2GP$0"]/text()')[0][1:].split(' ', 3)[0:3:2]).encode('ascii')
i = 0
while True:
    section_cat = tree.xpath('//a[@id="MTG_CLASSNAME$' + str(i) + '"]/text()')[0].split('-', 1)
    section = section_cat[0]
    datestring, timestring = tree.xpath('//span[@id="MTG_DAYTIME$' + str(i) + '"]/text()')[0].split(' ', 1)
    times = copy.deepcopy(default_times)
    timestrings = timestring.split(' ')
    for j in xrange(0, len(datestring), 2):
        times[datestring[j:j + 2]] += ((parsetime(timestrings[0]), parsetime(timestrings[2])),)
    try:
        campus = campus_map[tree.xpath('//span[@id="MTG_ROOM$' + str(i) + '"]/text()')[0].split()[-1]]
    except IndexError as e:
        if len(tree.xpath('//span[@id="MTG_ROOM$' + str(i) + '"]/text()')[0].split()) < 2:
            campus = NORTH
        else:
            raise e
    instructors = list(set(tree.xpath('//span[@id="MTG_INSTR$' + str(i) + '"]/text()')))
    instructor = '- '.join(map(lambda x: x.lstrip(), instructors))
    seats = tree.xpath('//span[@id="M_SR_DERIVED_AVAILABLE_SEATS$' + str(i) + '"]/text()')[0] + '/' + \
            tree.xpath('//span[@id="M_SR_DERIVED_WAIT_TOT$' + str(i) + '"]/text()')[0]
    reserved = tree.xpath('//span[@id="M_SR_RSVDFOR$' + str(i) + '"]/text()')[0]
    #print tree.xpath('//div[@id="win0divDERIVED_CLSRCH_SSR_STATUS_LONG$' + str(i) + '"]')[0].find('div').find('img').get("alt")
    new_name = name + '-' + section_cat[1]
    if not new_name in courses:
        courses[new_name] = {}
    priority = 0
    if int(seats.split('/', 1)[0]) == 0:
        priority = -100
    courses[new_name][section] = (times, campus, instructor, seats, reserved, priority)
    print i, courses[new_name][section]
    i += 1
    if not tree.xpath('//span[@id="MTG_ROOM$' + str(i) + '"]/text()'):
        break

#print courses

import json
#print json.dumps(courses, indent=4, separators=(',', ': '))

with open(name + '.json', 'w+') as f:
    f.seek(0)
    f.write(json.dumps(courses, sort_keys=True, indent=4, separators=(',', ': ')))
    f.truncate()
    f.close()
