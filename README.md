# Spreadsheet software: Static type checker
A static type inferencer and checker for a spreadsheet language to make sure that the spreadsheet values are appropriately typed. Made using Racket and miniKanren.

- Using racket to solve type checking problems.
- Use miniKanren and constraint logic programming to solve problems like type inference and type inhabitation.

# Requirements
- An IDE: I have used DrRacket which can be downloaded [here](https://download.racket-lang.org)

# Getting Started
### 1. Clone
  - Clone this repo to your local machine using: ``` git clone https://github.com/RohanPoojary1107/Spreadsheet-2.0.git ```

### 2. Run
  - Open the project files in DrRacket and run p2-spreadsheet.rkt
  
### 3. Sample usage
- To type check the following spreadsheet:

|Column | id  | name | age | voter | name2 | years-until-100|
| ------------- | ------------- | ------------- |------------- |------------- |------------- |------------- |
| Formula  | | | |(canvote age)|(concat name name)|(- 100 age)|
||1|"adam"|12|#f|“adamadam”|88|
||2|"betty"|15|#f|"bettybetty”|85|
||3|"clare"|18|#t|“clareclare”|82|
||4|"eric"|49|#t|“ericeric”|51|
||5|"sam"|17|#f|“samsam”|83|

- Represent this table in racket and input it to the function type-check-spreadsheet. 
```
(type-check-spreadsheet 
  '(spreadsheet
    (def (voting-age 18)
         (canvote (lambda (x) (>= x voting-age))))
    (columns
      (name str (values "adam" "betty" "clare" "eric" "sam")) 
      (age num (values 12 15 18 49 17))
      (voter bool (computed (canvote age)))
      (voter2 bool (computed (>= age name))))))
```
### Output
- ``` '(#t #t #t #f) ```
- This output shows that type checking succeeded for the first three columns, but not for the last one. The last column
uses the formula (>= age name) where name is a string rather than a number.

For more detailed information, refer to projectDoc.pdf
