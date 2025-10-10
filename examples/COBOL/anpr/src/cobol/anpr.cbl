*> The MIT License (MIT)
*> Copyright © 2022-2025 TS-Solution Corp.
*>
*> Permission is hereby granted, free of charge, to any person obtaining a copy
*> of this software and associated documentation files (the "Software"), to deal
*> in the Software without restriction, including without limitation the rights
*> to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*> copies of the Software, and to permit persons to whom the Software is
*> furnished to do so, subject to all conditions.
*>
*> The above copyright notice and this permission notice shall be included in all
*> copies or substantial portions of the Software.
*>
*> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*> IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*> FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*> AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*> LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*> OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*> SOFTWARE.

IDENTIFICATION DIVISION.
PROGRAM-ID. ANPR.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
REPOSITORY.
    FUNCTION ALL INTRINSIC.

DATA DIVISION.
WORKING-STORAGE SECTION.
*> Constants
01  WS-EXAMPLES-BASE-DIR    PIC X(256) VALUE "../../img".
01  WS-OUTPUT-FORMAT        PIC X(16) VALUE "text".

*> Buffer for ANPR results
01  WS-RESULT               PIC X(8192).
01  WS-RESULT-LEN           PIC 9(5) COMP VALUE 8192.
01  WS-RETURN-CODE          PIC S9(4) COMP.

*> Buffers for function parameters
01  WS-OPTIONS              PIC X(512).
01  WS-OPTIONS-LEN          PIC 9(4) COMP VALUE 512.
01  WS-IMAGE-PATH           PIC X(512).
01  WS-IMAGE-PATH-LEN       PIC 9(4) COMP VALUE 512.
01  WS-OUTPUT-FORMAT-LEN    PIC 9(4) COMP VALUE 16.

*> Country code
01  WS-COUNTRY-CODE         PIC X(2).

PROCEDURE DIVISION.

MAIN-PARA.
    DISPLAY "=== TS-ANPR COBOL Example ===".
    DISPLAY " ".

    *> TODO: Try each country code as needed
    MOVE "KR" TO WS-COUNTRY-CODE
    PERFORM READ-LICENSE-PLATES
    *> MOVE "JP" TO WS-COUNTRY-CODE
    *> PERFORM READ-LICENSE-PLATES
    *> MOVE "VN" TO WS-COUNTRY-CODE
    *> PERFORM READ-LICENSE-PLATES

    STOP RUN.

*> ========================================================================
*> READ-LICENSE-PLATES: Main processing for each country
*> ========================================================================
READ-LICENSE-PLATES.
    DISPLAY "=== Processing ", FUNCTION TRIM(WS-COUNTRY-CODE),
            " License Plates ===".
    DISPLAY " ".

    *> NOTICE:
    *> anpr_initialize should be called only once after library load.
    *> Therefore, it is not possible to change the country code
    *> after anpr_initialize has been called.
    *> While using the free trial license, you can try all languages.
    *> When you purchase a commercial license, you can only use the
    *> selected language.

    PERFORM INITIALIZE-ENGINE
    IF WS-RETURN-CODE NOT = 0 THEN
        DISPLAY "Failed to initialize ANPR engine"
        STOP RUN
    END-IF

    *> TODO: Try each output format as needed
    *> MOVE "json" TO WS-OUTPUT-FORMAT
    *> MOVE "yaml" TO WS-OUTPUT-FORMAT
    *> MOVE "xml" TO WS-OUTPUT-FORMAT
    *> MOVE "csv" TO WS-OUTPUT-FORMAT

    *> Single license plate recognition (default)
    PERFORM TEST-SINGLE-PLATE

    *> Multiple license plates recognition
    PERFORM TEST-MULTIPLE-PLATES

    *> Multiple license plates with motorcycles
    PERFORM TEST-MULTIPLE-WITH-MOTORCYCLES

    *> Surround detection tests
    PERFORM TEST-SURROUND-DETECTION
    PERFORM TEST-SURROUND-OBJECTS
    PERFORM TEST-SURROUND-WITH-RECOGNITION
    PERFORM TEST-SURROUND-WITH-ROI

    DISPLAY " ".

*> ========================================================================
*> INITIALIZE-ENGINE: Initialize the ANPR engine with country code
*> ========================================================================
INITIALIZE-ENGINE.
    STRING "text;country=" FUNCTION TRIM(WS-COUNTRY-CODE)
        DELIMITED BY SIZE INTO WS-OPTIONS
    END-STRING

    CALL "tsanpr_cobol_initialize" USING
        BY REFERENCE WS-OPTIONS
        BY VALUE WS-OPTIONS-LEN
        BY REFERENCE WS-RESULT
        BY VALUE WS-RESULT-LEN
        RETURNING WS-RETURN-CODE
    END-CALL

    IF WS-RETURN-CODE NOT = 0 THEN
        DISPLAY "anpr_initialize() failed (error=",
                FUNCTION TRIM(WS-RESULT), ")"
    END-IF.

*> ========================================================================
*> TEST-SINGLE-PLATE: Test single license plate recognition
*> ========================================================================
TEST-SINGLE-PLATE.
    MOVE SPACES TO WS-IMAGE-PATH
    STRING
        FUNCTION TRIM(WS-EXAMPLES-BASE-DIR) "/"
        FUNCTION TRIM(WS-COUNTRY-CODE) "/licensePlate.jpg"
        DELIMITED BY SIZE INTO WS-IMAGE-PATH
    END-STRING
    MOVE SPACES TO WS-OPTIONS
    PERFORM CALL-ANPR-READ-FILE.

*> ========================================================================
*> TEST-MULTIPLE-PLATES: Recognize multiple license plates on vehicles
*> ========================================================================
TEST-MULTIPLE-PLATES.
    MOVE SPACES TO WS-IMAGE-PATH
    STRING
        FUNCTION TRIM(WS-EXAMPLES-BASE-DIR) "/"
        FUNCTION TRIM(WS-COUNTRY-CODE) "/multiple.jpg"
        DELIMITED BY SIZE INTO WS-IMAGE-PATH
    END-STRING
    MOVE "vm" TO WS-OPTIONS
    PERFORM CALL-ANPR-READ-FILE.

*> ========================================================================
*> TEST-MULTIPLE-WITH-MOTORCYCLES: Include motorcycles
*> ========================================================================
TEST-MULTIPLE-WITH-MOTORCYCLES.
    MOVE SPACES TO WS-IMAGE-PATH
    STRING
        FUNCTION TRIM(WS-EXAMPLES-BASE-DIR) "/"
        FUNCTION TRIM(WS-COUNTRY-CODE) "/multiple.jpg"
        DELIMITED BY SIZE INTO WS-IMAGE-PATH
    END-STRING
    MOVE "vmb" TO WS-OPTIONS
    PERFORM CALL-ANPR-READ-FILE.

*> ========================================================================
*> TEST-SURROUND-DETECTION: Surround license plate detection
*> ========================================================================
TEST-SURROUND-DETECTION.
    MOVE SPACES TO WS-IMAGE-PATH
    STRING
        FUNCTION TRIM(WS-EXAMPLES-BASE-DIR) "/"
        FUNCTION TRIM(WS-COUNTRY-CODE) "/surround.jpg"
        DELIMITED BY SIZE INTO WS-IMAGE-PATH
    END-STRING
    MOVE "vms" TO WS-OPTIONS
    PERFORM CALL-ANPR-READ-FILE.

*> ========================================================================
*> TEST-SURROUND-OBJECTS: Detect multiple surrounding objects (vehicles)
*> ========================================================================
TEST-SURROUND-OBJECTS.
    MOVE SPACES TO WS-IMAGE-PATH
    STRING
        FUNCTION TRIM(WS-EXAMPLES-BASE-DIR) "/"
        FUNCTION TRIM(WS-COUNTRY-CODE) "/surround.jpg"
        DELIMITED BY SIZE INTO WS-IMAGE-PATH
    END-STRING
    MOVE "dms" TO WS-OPTIONS
    PERFORM CALL-ANPR-READ-FILE.

*> ========================================================================
*> TEST-SURROUND-WITH-RECOGNITION: Objects and license plates
*> ========================================================================
TEST-SURROUND-WITH-RECOGNITION.
    MOVE SPACES TO WS-IMAGE-PATH
    STRING
        FUNCTION TRIM(WS-EXAMPLES-BASE-DIR) "/"
        FUNCTION TRIM(WS-COUNTRY-CODE) "/surround.jpg"
        DELIMITED BY SIZE INTO WS-IMAGE-PATH
    END-STRING
    MOVE "dmsr" TO WS-OPTIONS
    PERFORM CALL-ANPR-READ-FILE.

*> ========================================================================
*> TEST-SURROUND-WITH-ROI: Objects and plates within Region of Interest
*> ========================================================================
TEST-SURROUND-WITH-ROI.
    MOVE SPACES TO WS-IMAGE-PATH
    STRING
        FUNCTION TRIM(WS-EXAMPLES-BASE-DIR) "/"
        FUNCTION TRIM(WS-COUNTRY-CODE) "/surround.jpg"
        DELIMITED BY SIZE INTO WS-IMAGE-PATH
    END-STRING
    MOVE "dmsri549,700,549,2427,1289,2427,1289,700" TO WS-OPTIONS
    PERFORM CALL-ANPR-READ-FILE.

*> ========================================================================
*> CALL-ANPR-READ-FILE: Call the C wrapper to read and process image
*> ========================================================================
CALL-ANPR-READ-FILE.
    DISPLAY FUNCTION TRIM(WS-IMAGE-PATH),
            " (outputFormat=""", FUNCTION TRIM(WS-OUTPUT-FORMAT),
            """, options=""", FUNCTION TRIM(WS-OPTIONS), """) => "
            WITH NO ADVANCING
    END-DISPLAY

    CALL "tsanpr_cobol_read_file" USING
        BY REFERENCE WS-IMAGE-PATH
        BY VALUE WS-IMAGE-PATH-LEN
        BY REFERENCE WS-OUTPUT-FORMAT
        BY VALUE WS-OUTPUT-FORMAT-LEN
        BY REFERENCE WS-OPTIONS
        BY VALUE WS-OPTIONS-LEN
        BY REFERENCE WS-RESULT
        BY VALUE WS-RESULT-LEN
        RETURNING WS-RETURN-CODE
    END-CALL

    IF WS-RETURN-CODE = 0 THEN
        DISPLAY FUNCTION TRIM(WS-RESULT)
    ELSE
        DISPLAY "ERROR: ", FUNCTION TRIM(WS-RESULT)
    END-IF.

END PROGRAM ANPR.