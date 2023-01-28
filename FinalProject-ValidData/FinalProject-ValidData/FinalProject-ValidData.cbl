       identification division.
       program-id. FinalProject-ValidData.
       author. David Osagiede.
       date-written. 2022-03-31.
      *Program Description: This program is the #1 program in the 
      *project. It will read records from a input file & then
      *display a error report + any invalid & valid records
      *in a seperate output files
      *
       environment division.
       configuration section.
       select input-file
                assign to '../../../data/project6.dat'
                organization is line sequential.
      *
           select valid-file
                assign to '../../../data/valid.out'
                organization is line sequential.
      *
           select invalid-file
                assign to '../../../data/invalid.out'
                organization is line sequential.
      *
           select error-file
                assign to '../../../data/error-report.out'
                organization is line sequential.
       data division.
       file section.
              fd input-file
                 record contains 36 characters
                 data record is emp-record.
      *records for the invalid, valid, error report output files
       01 emp-record.
         05 emp-transac-code             pic x.
         05 emp-transac-amount           pic 9(5)V99.
         05 emp-payment-type             pic xx.
         05 emp-store-number             pic 99.
         05 emp-invoice-number.
           10 emp-inv-num-pfx-char-1-2.
             15 emp-inv-num-pfx-char1    pic x.
             15 emp-inv-num-pfx-char2    pic x.
           10 emp-inv-num-hyphen         pic x.
             88 input-inv-num-is-hyphen
               value '-'.
           10 emp-inv-num-base-part      pic 9(6).
      *        
         05 emp-sku-code                 pic x(15).
       fd valid-file
           record contains 36 characters
           data record is ws-valid-line.
      *
       01 val-valid-line.
         05 val-transac-code             pic x.
         05 val-transac-amount           pic 9(5)V99.
         05 val-payment-type             pic xx.
         05 val-store-number             pic 99.
         05 val-invoice-number           pic x(9).
         05 val-sku-code                 pic x(15).
      *
       fd invalid-file
           record contains 36 characters
           data record is ws-invalid-line.
      *
       01 inv-invalid-line.
         05 inv-transac-code             pic x.
         05 inv-transac-amount           pic 9(5)V99.
         05 inv-payment-type             pic xx.
         05 inv-store-number             pic 99.
         05 inv-invoice-number           pic x(9).
         05 inv-sku-code                 pic x(15).
      *  
       fd error-file
           record contains 339 characters
           data record is ws-error-line.
      *
       01 error-line pic x(339).
      *
       working-storage section.
       01 ws-error-line.
         05 ws-transac-code              pic x.
         05 ws-transac-amount            pic 9(5)V99.
         05 ws-payment-type              pic xx.
         05 ws-store-number              pic 99.
         05 ws-invoice-number            pic x(9).
         05 ws-sku-code                  pic x(15).
         05 filler                       pic x(3)
             value spaces.
         05 ws-errors-out                pic x(300).
       01 ws-errors-tbl.
         05 ws-errors                    pic x(30) occurs 10 times.
         05 filler                       pic x(39)
             value spaces.
       
      *records used for errors, variables calculations & detail lines
      * 
       01 ws-flags.
         05 ws-eof-flag                  pic x
             value space.
         05 ws-error-flag                pic x
             value space.
      *
       01 ws-boolean-cnst.
         05 ws-true-cnst                 pic x
             value "T".
         05 ws-false-cnst                pic x
             value "N".
      *
       01 calculate-lines.
         05 invoice-split-num            pic 9(6).
       
         05 error-sum                    pic 9(2).
         05 ws-total-valid               pic 9(3)
             value 0.
         05 ws-total-invalid             pic 9(3)
             value 0.
      *
       01 ws-heading.
         05 filler                       pic x(15) 
             value "Raw Input Data:".
         05 filler                       pic x(24)
             value spaces.
         05 filler                       pic x(14)                  
             value "Error Message:".
         05 filler                       pic x(286)
             value spaces.
      *
       01 ws-error-totals.
         05 ws-valid-recs                pic x(20)
             value "Total valid records: ".
         05 ws-valid-total               pic ZZ9.
         05 filler                       pic x(3)
             value spaces.
         05 ws-valid-recs                pic x(22)                  
             value "Total invalid records: ".
         05 ws-invalid-total             pic ZZ9.
         05 filler                       pic x(3)
             value spaces.
         05 ws-total-recs                pic x(14)                  
             value "Total records: ".
         05 ws-total                     pic ZZ9.
         05 filler                       pic x(268)
             value spaces.
      *
       procedure division.
       000-main.
      *  performs & processes input file that leads to the 
      *  valid, invalid & error report files
           open input input-file,
             output valid-file, invalid-file, error-file.
           move spaces to error-line.
      * writes heading for the raw input data & error message 
           write error-line from ws-heading before advancing 2 lines.
      *      
           read input-file
               at end
                   move "y" to ws-eof-flag.
           perform 100-process-summary until ws-eof-flag = "y".
      * 
           perform 600-total-amounts
           close input-file,
             valid-file, invalid-file, error-file.
      *      
           display "Press enter to continue".
           accept return-code.
           goback.
           stop run.
      *performs the paragraphs that allow for validation of data
       100-process-summary.
           move ws-false-cnst to ws-error-flag.
      *
           perform 200-process-input.
      *
           if ws-error-flag = ws-true-cnst then
               perform 400-invalid-data
               perform 500-error-summary
               add 1 to ws-total-invalid giving ws-total-invalid
           else
               perform 300-valid-data
               add 1 to ws-total-valid giving ws-total-valid
           end-if.
           read input-file
               at end
                   move "y" to ws-eof-flag.
      * validation for errors processed
       200-process-input.
           
               move ws-false-cnst     to ws-error-flag.
               move spaces            to ws-errors-tbl.
      *payment is 'CA', 'CR', 'DB'
           if not (emp-payment-type = 'CA' or 'CR' or 'DB') then
      *
               move ws-true-cnst      to ws-error-flag
               add 1                  to error-sum giving error-sum
               move "Payment Invalid" to ws-errors(error-sum)
           end-if.
      *transcation amount must be 'S', 'R', 'L'
           if not (emp-transac-code = 'S' or 'R' OR 'L') then
      *
               move ws-true-cnst      to ws-error-flag
               add 1                  to error-sum giving error-sum
               move "transaction code wrong"
                                      to ws-errors(error-sum)
           end-if.
      * if the store number is not as intended (1,2,3,4,5,12)
           if not (emp-store-number = 1 or 2 or 3 or 4 or 5 or 12) then
      *
               move ws-true-cnst      to ws-error-flag
               add 1                  to error-sum giving error-sum
               move "wrong store number"
                                      to ws-errors(error-sum)
           end-if.
      *if records dont have a dash ‘-‘ in position 3 of invoice number
      *    if not emp-inv-num-hyphen = '-' then
           if not input-inv-num-is-hyphen then
      *
               move ws-true-cnst      to ws-error-flag
               add 1                  to error-sum giving error-sum
               move "invoice needs a dash - "
                                      to ws-errors(error-sum)
           end-if.
      *check if Invoice Number XX/XY is A or B or C or D or E
           if not ((emp-inv-num-pfx-char1 equals 'A' or 'B' or 'C' or
             'D' or 'E')  AND
             (emp-inv-num-pfx-char2 equals 'A' or 'B' or 'C' or 'D' or
             'E')) then
               move ws-true-cnst      to ws-error-flag
               add 1                  to error-sum giving error-sum
               move "Invoice XY Invalid" to ws-errors(error-sum)
           end-if.
      *
      *if invoice number pfx char 1 2 is not a 'AA' 'BB' 'CC' 'DD' 'EE'
           if (emp-inv-num-pfx-char-1-2 equals 'AA' or 'BB' or 'CC' or
             'DD' or 'EE') then
               move ws-true-cnst      to ws-error-flag
               add 1                  to error-sum giving error-sum
               move "Invoice XX Invalid"
                                      to ws-errors(error-sum)
           end-if.
      *

         move emp-invoice-number(4:6) to invoice-split-num
      * if invoice not between 900000 - 100000
           if not invoice-split-num >= 100000 and <= 900000 then
      *
               move ws-true-cnst      to ws-error-flag
               add 1                  to error-sum giving error-sum
               move "invalid invoice too high/low"
                                      to ws-errors(error-sum)
           end-if.
      *The SKU field is X(15), so it must be alphanumeric already.
      *The edit is to check to see that is not empty(spaces).
           if emp-sku-code = spaces then
               move ws-true-cnst      to ws-error-flag
               add 1                  to error-sum giving error-sum
               move "sku code cant be empty-spaces"
                                      to ws-errors(error-sum)
           end-if.
      * if the transacation is not numeric
          if emp-transac-amount is not numeric then
               move ws-true-cnst      to ws-error-flag
               add 1                  to error-sum giving error-sum
               move "Transaction must be numeric"
                                      to ws-errors(error-sum)
           end-if.
      *
      * moves valid, invalid, and error data validation to error line
       300-valid-data.
      *    
           write val-valid-line from emp-record.
      *      
       400-invalid-data.
      *      
           write inv-invalid-line from emp-record.
      *      
       500-error-summary.
      * allows for spaces between the lines
           move spaces to error-line.
      *
           move emp-invoice-number    to ws-invoice-number.
           move emp-payment-type      to ws-payment-type.
           move emp-sku-code          to ws-sku-code.
           move emp-store-number      to ws-store-number.
           move emp-transac-amount    to ws-transac-amount.
           move emp-transac-code      to ws-transac-code.
      *    
           move ws-errors-tbl         to ws-errors-out.
      *
           write error-line from
             ws-error-line before advancing 2 lines.
      *
           move 0 to error-sum.
      *
      * gives the amount for invalid/valid and total records
       600-total-amounts.
      *
           move spaces to error-line.

           move ws-total-valid        to ws-valid-total.
           move ws-total-invalid      to ws-invalid-total.
           add ws-total-invalid       to ws-total-valid giving
             ws-total.
      *
           write error-line from ws-error-totals.
      *
       end program FinalProject-ValidData.