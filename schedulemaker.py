#!/usr/bin/python

import json

CENTRAL = 0
NORTH = 1

courses = {}
priorities = {}
time_names = ['Mo', 'Tu', 'We', 'Th', 'Fr']
default_times = {'Mo': [], 'Tu': [], 'We': [], 'Th': [], 'Fr': []}



course_names = ["LING210",
                "STATS250",
                "EECS281",
                "PHYSICS240",
                "PHYSICS241"]

starting_campus = CENTRAL



for s in course_names:
    with open(s + '.json', 'r') as f:
        courses.update(json.load(f))
#print courses
    
    
def output_schedule(schedule):
    times = [[], [], [], [], []]
    switches = 0
    last_campus = starting_campus
    priority = 0;
    for i in schedule:
        a = schedule[i].itervalues().next()
        itimes = a[0]
        for j, s in enumerate(time_names):
            if itimes[s]:
                itimes[s].sort()
                times[j] += [(k, a[1]) for k in itimes[s]]
                if itimes[s][-1][1] + 50 * (a[1] != starting_campus) > 1800:
                    priority -= 10
                if itimes[s][0][0] - 50 * (a[1] != starting_campus) < 900:
                    priority -= 10
                if itimes[s][0][0] - 50 * (a[1] != starting_campus) < 950:
                    priority -= 5
                #if s == 'We' and itimes[s][-1][1] > 1500:
                #    return
                #if s == 'Fr' and itimes[s][-1][1] > 1400:
                #    return
                if s == 'Fr':
                    priority -= (itimes[s][-1][1] - itimes[s][0][0]) / 8 + 7 * (a[1] != starting_campus)
    times = map(sorted, times)
    for i in times:
        for j in i:
            switches += j[1] != last_campus
            last_campus = j[1]
        switches += last_campus != starting_campus
        last_campus = starting_campus
    if switches > 6:
        return
    for i in schedule:
        priority += schedule[i].iteritems().next()[1][5]
        if schedule[i].iteritems().next()[1][4] != "N/A":
            return
        #print schedule[i].iteritems().next()[1]
        #if int(schedule[i].iteritems().next()[1][3].split('/', 1)[0]) == 0:
            #return
    if priority not in priorities:
        priorities[priority] = 1
    else:
        priorities[priority] += 1
    if priority < -31:
        return
    for i in schedule:
        print i, schedule[i].iterkeys().next(), '--', '; '.join(schedule[i].iteritems().next()[1][2:5])
    for i, s in enumerate(time_names):
        print '\t', s, times[i]
    print "switches:", switches
    print "priority:", priority
    print
    

def try_schedule(course, schedule):
    for name2 in schedule:
        course2 = schedule[name2].itervalues().next()
        travel_time = 50 * (course[1] != course2[1])
        for day in default_times:
            for timerange in course[0][day]:
                for timerange2 in course2[0][day]:
                    if timerange[0] - travel_time < timerange2[1] and \
                       timerange2[0] - travel_time < timerange[1]:
                        return False
    return True

def schedule_search(courses, schedule):
    if courses:
        course = courses.popitem()
        for section in course[1]:
            if try_schedule(course[1][section], schedule):
                schedule[course[0]] = {section: course[1][section]}
                schedule_search(courses, schedule)
                schedule.pop(course[0])
        courses[course[0]] = course[1]
    else:
        output_schedule(schedule)
        

schedule_search(courses, {})

print
print priorities
print max(priorities.keys())