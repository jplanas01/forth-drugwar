254 Constant max-line-len

: open-config ( -- addr )
	s" config.txt" r/o open-file throw
;

\ Iterate over the string until we run out of string or
\ find a non-matching character, return number of matches
: char-span { addr len chara -- len }
	0 addr len 0
	?do
		dup c@ chara
		<> if
			leave
		then
		char+
		swap 1+ swap
	loop
	drop
;


\ Iterate over the string until we run out of string or
\ find a matching character, return number of non-matches
: until-char-span ( addr len char -- len )
	-rot 0 0 -rot
	?do
		over c@ 3 pick
		= if
			leave
		then
		1+
		swap char+ swap
	loop
	-rot drop drop
;

\ Advance a pointer past consecutive characters
: skip-chars { addr len sep -- new-addr new-len }
	addr len sep char-span
	dup chars addr + swap
	len swap -
;

\ Advance a pointer until a character
: skip-until { addr len sep -- new-addr new-len }
	addr len sep until-char-span
	dup chars addr + swap
	len swap -
;

: strtok { hay-addr hay-len sep  -- new-haystack new-haystack-len token-addr token-len }
	\ pseudo-strtok, find tokenized strings delimited by one or more characters
	\ Note, new-haystack points at character after end of token (ie, the first delimiter found)
	hay-addr hay-len sep skip-chars

	dup 0 <= if
		drop drop hay-addr hay-len 0 0
	else
		2dup sep skip-until
		rot over - -rot \ Make token length actual length instead of 
		                \ length of string starting at token
		2swap
	then
;	

: strdup-heap { addr len -- new-addr len }
	len allocate throw
	dup addr swap len cmove
	len
;

: get-counted-pair ( list-addr elem-num -- addr len )
	2 * cells 1 cells +
	+ 2@
;

: store-counted-pair ( num1 num2 list-addr elem-num )
	2 * cells 1 cells +
	+ 2!
;

: get-2counted-pair ( list-addr elem-num -- addr len )
	2 * cells 2 cells +
	+ 2@
;

: store-2counted-pair ( num1 num2 list-addr elem-num )
	2 * cells 2 cells +
	+ 2!
;

: get-pair ( list-addr elem-num -- num1 num2 )
	2 * cells + 2@
;

: store-pair ( num1 num2 list-addr elem-num -- )
	2 * cells + 2!
;

: get-elem ( addr elem-num -- elem )
	cells + @
;

: set-elem ( elem addr elem-num -- )
	cells + !
;

\ Save drug names into array  of [len, string address] pairs
\ starting 1 cell after list-addr
\ list-addr itself will contain count of drugs
: parse-header { buffer-addr buffer-len list-start -- }
	buffer-addr buffer-len bl strtok
	begin
		strdup-heap 2,
		1 list-start +!
		dup 0 >
	while
		bl strtok
	repeat
	drop drop
;

: read-city-name { buffer-addr buffer-len -- rest-addr rest-len city-addr city-len }
	buffer-addr buffer-len ':' strtok
	strdup-heap
	\ Advance buffer pointer past ':'
	2swap swap 1 chars + swap 2swap
;

\ 2 cells for city name pointer and length and 2 cells for high/low for each drug 
: alloc-city-heap ( num-drugs -- addr )
	1 + 2 * cells allocate throw
;

: get-price ( buffer-addr buffer-len -- rest-addr rest-len num1 num2 )
	bl strtok
	s>number? 2drop -rot
	bl strtok
	s>number? 2drop -rot
	2swap
	\ TODO order num1 and num2 suck that low number is lower on stack
;

: process-city ( buf-addr buf-len num-drugs -- city-addr )
	dup >r
	alloc-city-heap -rot
	read-city-name
	4 pick 2!
	r> 0
	?do
		get-price
		4 pick i store-2counted-pair
	loop
	2drop
;

: load-cities { num-drugs line-buf max-line fd city-list -- city-num }
	0
	begin
		line-buf max-line fd read-line throw
		\ flag on top of stack, chars read below that
	while
		line-buf swap num-drugs process-city
		,
		1+
	repeat
	\ Extra 0 at top of stack -- where did it come from?
	drop
	city-list !
;

: free-strings { drug-list city-list -- }
	drug-list @ 0
	?do
		drug-list i get-counted-pair drop free throw
		0 0 drug-list i store-counted-pair
	loop

	city-list @ 0
	?do
		city-list i 1 + cells + @
		dup 2@ swap free throw
		drop free throw
		0 city-list i 1 + cells + !
	loop
;

: zero-array ( addr len -- )
	0
	?do
		0 over i set-elem
	loop
	drop
;

: print-location ( city-list city-num -- )
	s" Currently in: " type
	1 + get-elem
	0 get-pair type
;

: print-inventory { drug-list inventory -- }
	drug-list @ 0
	?do
		i . 8 emit s" ) " type
		drug-list i get-counted-pair type
		s" : " type
		inventory i get-elem .
	loop
;

: print-prices { drug-list prices -- }
	drug-list @ 0
	?do
		i . 8 emit s" ) " type
		drug-list i get-counted-pair type
		s" : " type
		prices i get-elem .
	loop
;

: print-status { drug-list city-list city day cash inventory prices -- }
	s" Day: " type day @ . cr
	city-list city @ print-location cr
	s" Cash: " type cash @ . cr
	s" Inventory: " type drug-list inventory print-inventory cr
	s" Today's prices: " type drug-list prices print-prices cr
;

: print-cities ( city-list -- )
	dup 0 get-elem
	0 ?do
		dup
		i . 8 emit s" ) " type
		i 1 + get-elem
		0 get-pair type bl emit
	loop
	drop
;

: get-stdin-num ( -- num )
	pad 5 accept
	pad swap s>unumber? 2drop
;

: jet { city-list city -- }
	city-list print-cities cr
	s" Where to, pal? " type
	\ TODO
	\ Add error checking at some point
	get-stdin-num dup
	
	city-list @ < if
		s" Let's go!" type cr
		city !
		true
	else
		s" I don't think so, Tim." type cr
		false
	then
;

\ TODO actually implement
: rand-int { low high -- rand }
	high low + 2 /
;

: update-prices { city-list city prices drug-num -- }
	city-list city @ 1 + cells + @
	drug-num 0
	?do
		dup i 
		get-2counted-pair \ stack: low high
		rand-int
		prices i set-elem
	loop
	drop
;

: do-purchase { drug quantity cost inventory cash -- }
	cash @ cost - cash !
	inventory drug get-elem quantity + 
	inventory drug set-elem
;

: buy { inventory prices cash drug-num -- }
	s" Which one you want, homes? " type
	get-stdin-num dup dup
	drug-num < if
		prices swap get-elem
		s" How many? " type get-stdin-num
		swap over * dup
		cash @ > if
			s" You can't afford that." type cr
		else
			inventory cash do-purchase
		then
	else
		s" We don't got that one." type cr
	then
;

: do-sale { drug quantity prices inventory cash -- }
	prices drug get-elem quantity *
	cash @ + cash drug set-elem
	inventory drug get-elem quantity -
	inventory drug set-elem
;

: sell { inventory prices cash drug-num -- success? }
	s" Which one to sell, homes? " type
	get-stdin-num dup 
	drug-num < if
		s" How many? " type get-stdin-num
		over inventory swap get-elem
		over 
		< if
			s" You don't have that many." type cr
		else
			prices inventory cash do-sale
		then
	else
		s" That drug doesn't exist." type cr
	then
;

: game-loop { city-list drug-list day cash city inventory prices -- advance-day? }
	cr
	s" What to do? (b = buy, s = sell, j = jet, q = quit): " type
	key dup emit cr
	case
		'j' of
			city-list city jet	
		endof
		'b' of
			inventory prices cash drug-list @ buy
			false
		endof
		's' of
			inventory prices cash drug-list @ sell
			false
		endof
		'q' of
			'q'
		endof
		false
	endcase
;

: run-day { city-list drug-list day cash city inventory prices -- }
	1 day +!
	city-list city prices drug-list @ update-prices
	drug-list city-list city day cash inventory prices print-status

	begin
		city-list drug-list day cash city inventory prices game-loop
		dup 'q' = if
			exit
		then
		invert
	while
	repeat
;

open-config Value fd-in
Create line-buffer max-line-len 2 + allot
line-buffer max-line-len fd-in read-line throw
Value flag Value chars-read

create drug-list 0 ,
line-buffer chars-read drug-list parse-header
drug-list @ value num-drugs

create city-list 0 ,
num-drugs line-buffer max-line-len fd-in city-list load-cities

align
variable day
variable cash
variable city
0 day ! 10000 cash ! 0 city !
\ Has to be in this order (create and then a word) or it crashes with undefined word -- why???
create inventory num-drugs cells allot
inventory num-drugs zero-array
create prices num-drugs cells allot
prices num-drugs zero-array

city-list drug-list day cash city inventory prices run-day
drug-list city-list free-strings
