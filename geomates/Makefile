
CC    	= clang # or gcc 
FLAGS   = -I /Users/diedrich/Software/box2d/include/
CFLAGS  = -pedantic -Wall
LDFLAGS = -L /Users/diedrich/Software/box2d/build/src/ -lbox2d -shared -fPIC

TARGET  = wrapper.so
SOURCE  = wrapper.c

all:
	$(CC) $(FLAGS) $(CFLAGS) $(LDFLAGS) -o $(TARGET) $(SOURCE)
