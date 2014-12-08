Programming Project 3: Propositional-logic
CS 710 - AI
12/7/2014
Nyles Breecher

========== Contents =========

Introduction
Files Included
Notes About My Program
Limitations

========== Introduction =========

Assignment 3 for my AI course.  Implements various functions to work with propositional logic statements with the end goal of developing enough representations to assist in a planning problem using STRIPS representation.

========== Files Included =========

prositional-logic/
    main.lisp
    mp3_demo
    README.md
    STRIPS.txt
    test-inputs/
        TruthValue-tests.txt
        wfp-checker-tests.txt
        wfp-checkerFOL-tests.txt

main.lisp: This file contains all the function definitions asked for in parts a-d of the assignment (wfp-checker, TruthValue, and wfp-checkerFOL).  It does not do anything on its own.  It contains internal comments to describe my methods.

mp3_demo: This is a lisp file which loads main.lisp, runs a test suite for each of the functions written in main.lisp, then outputs the input worked on and the result obtained by using a particular function on it.

README.md: This file.

STRIPS.txt: This is my by hand (no programming) STRIPS representation for the monkey and banana problem as outlined in part e of our assignment.  It has three parts: a nice descriptive strips representation, a translation of that into the first order logic conditions as outline in part b of our assignment, and then my proposed plan for how the problem may be solved.

test-inputs/ : This folder contains all the test cases for the program.  The formmating for tests is one input per line, and mp3_demo will iterate over each line in the files contained here.  Feel free to add your own test cases to these files to see the results!  Each file contains a line break between inputs which should be true and inputs which should be false.

    TruthVale-tests.txt: Contains tests for TruthValue (part b).

    wfp-checker-tests.txt: Contains tests for wfp-checker (part a).

    wfp-checkerFOL-tests.txt: Contains tests for wfp-checkerFOL (part c).

========== Notes About My Program =========

I have tried my best to write a readable Lisp program, but as per the other assignments there are places when I'm sure things might be tough to follow.  In general the algorithms for my functions just follows a bunch of cascading check rules.  It examines part of the proposition to see if it fits a certain propery, and if not moves onto the next part.

For each function I have tried to include tests in my test files to illustrate certain properties of a proposition which my functions detect.  For example, wfp-checker makes sure that I have a the proper number of arguments for each of the operators like NOT or AND.  I have not specifically written down in words what each case tests, but hopefully it should be (relatively) clear what types of things are detected.

If you would like to test my program, simply load the mp3_demo file in a Lisp REPL and it will automatically run my test cases and do output formatting for you.  If you would like to check particular cases, I recommend adding them into the tests files in the test-inputs folders, but you could run from the command line directly as well.

Note: wfp-checker and wfp-checkerFOL only take strings as inputs.  The reason is that ill-formed propositions, something like (P with a missing closing parenthesis will screw up Lisp processing, so converting an input to a string and then verifying it is a wfp from it being a string is ideal.  TruthValue works just fine with lists though, because it assumes that the input is already well-formed.

========== Limitations =========

I think I did a good job making my functions properly check for wfps, but there are a lot of possible test cases so there are certainly things that I missed.   In particular I can see this happening for things which might lack white spaces.  I tried to account for this, but did not test all situations.

Additionally, I had some trouble interpreting the directions for part d of the problem assignment.  In part d-1 the instruction "extend the logic above" was particularly vague to me.  I wasn't exactly sure the specifics of how this was supposed to be done.  Something to just to check for valid STRIPS represenations (of whatever defined rules)? Or actual operators as functions in LISP which one can call to change the state the computer stores and verify if the goal has been reached.  To accomodate for this confusion I have written up the STRIPS problem (in STRIPS.txt) with a way I would model the problem with STRIPS represenation, even though it doesn't connect directly to any code.