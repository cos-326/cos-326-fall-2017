SEQUENCE=Parallel.fsi Parallel.fs Sequential.fsi Sequential.fs Sequence.fsi Sequence.fs 
PROGRAM=$(SEQUENCE) Bench.fs Counter.fsi Counter.fs Words.fs Program.fs

all: A7.exe

A7.exe: $(PROGRAM)
	fsharpc --tailcalls -O  $(PROGRAM) -o A7.exe

rbench: A7.exe
	mono A7.exe

jingle: A7.exe
	mono A7.exe data/holidays jingle

dreidel: A7.exe
	mono A7.exe data/holidays dreidel

flurries: A7.exe
	mono A7.exe data/holidays flurries

clean:
	rm -f A7.exe FSharp.Core.dll
