
run:
	stack exec -- runhaskell -i./src src/Main.hs

runhaskell:
	for i in src/Set1/Challenge_*.hs; do stack exec -- runhaskell -i./src $$i ; done
