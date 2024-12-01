BIN_LOC=bin/day_$(day)
SRC_LOC=src/day_$(day).hs
DATA_LOC=data/day_$(day).txt

default:
	ghc -outputdir=out -o $(BIN_LOC) $(SRC_LOC) && $(BIN_LOC) $(DATA_LOC)
