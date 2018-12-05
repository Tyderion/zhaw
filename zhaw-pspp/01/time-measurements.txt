#non-parallel
.\timeit.exe .\pi.exe
 An approximation of pi after
    50000000  iterations is
   3.14126182

Version Number:   Windows NT 6.2 (Build 9200)
Exit Time:        1:47 pm, Saturday, September 22 2018
Elapsed Time:     0:00:03.744
Process Time:     0:00:03.734
System Calls:     241635
Context Switches: 134562
Page Faults:      189174
Bytes Read:       210972
Bytes Written:    3421678

#parallel
.\timeit.exe .\pip.exe
 An approximation of pi after
     1000000  iterations is
   3.14102006

Version Number:   Windows NT 6.2 (Build 9200)
Exit Time:        1:46 pm, Saturday, September 22 2018
Elapsed Time:     0:00:08.002
Process Time:     0:00:06.296
System Calls:     4469009
Context Switches: 4278195
Page Faults:      396565
Bytes Read:       366670
Bytes Written:    383993
Bytes Other:      8405635

# parallel ran0
 .\timeit.exe .\pipr.exe
 An approximation of pi after
    50000000  iterations is
   3.13733268

Version Number:   Windows NT 6.2 (Build 9200)
Exit Time:        1:44 pm, Saturday, September 22 2018
Elapsed Time:     0:00:01.229
Process Time:     0:00:04.671
System Calls:     92108
Context Switches: 46387
Page Faults:      62374
Bytes Read:       112972
Bytes Written:    88394
Bytes Other:      1314465


# Summary
Serial: 3.744s for 50'000'000 iterations
para:    8.002s for 1'000'000 iterations
ran0:   1.229s for 50'000'000 iterations


The big difference is because of a lot more context switches, system calls and pagefaults on the original parallel implementation with rand. 
This gets completely "fixed" with ran0.