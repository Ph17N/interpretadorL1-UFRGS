OC = ocamlopt
SRC = src
BIN = bin
OBJ = obj
INCLUDE = include
INC = -I $(INCLUDE) -I $(OBJ)
OBJECTS = $(OBJ)/Syntax.cmx $(OBJ)/Semantics.cmx $(OBJ)/TypeSystem.cmx $(OBJ)/main.cmx
INTERFACES =
NAME = interpretadorL1

all: $(OBJ) $(BIN) $(INCLUDE) $(INTERFACES) $(OBJECTS)
	$(OC) $(INC) -o $(BIN)/$(NAME) $(OBJECTS)

$(INCLUDE):
	mkdir $(INCLUDE)
$(OBJ):
	mkdir $(OBJ)
$(BIN):
	mkdir $(BIN)

$(OBJ)/%.cmi:$(INCLUDE)/%.mli
	$(OC) -o $@ -c $<

$(OBJ)/%.cmx:$(SRC)/%.ml
	$(OC) -o $@ -c $(INC) $<

clean:
	rm -f $(OBJ)/*
	rm -f $(BIN)/$(NAME)