BEGIN \
{
  # M[] memory
  # data stack S[]+pS T I R[]+pR return stack
  # P program counter
  # H dict ptr
  # D current dict

  # init
  H = 0; FORTH = 0; MACRO = 1;
  # init dict
  InitDict();

  # init
  P = 0; XP = 1000000; pR = 0; pS = 0; STATE = 0; D = FORTH;
  HexOut = 0;
  # run
  print "Basic FORTH";
  Refill();
  DoInterpret();
}

function Refill(rc)
{
  rc = getline;
  if (rc < 1) {
    printf "getline(): rc=%d\n",rc; exit(0);
  }
  DollI = 1;
}

function Prim(tok,nomac)
{
  # Compile("\"" tok "\"");
  Dict[FORTH,tok] = H;
  # printf "--- FORTH %d: %s\n",H,tok;
  Compile(tok);
  Compile("RET");

  if (nomac) return;

  # Compile("\"" tok "\"");
  Dict[MACRO,tok] = H;
  # printf "--- MACRO %d: %s\n",H,tok;
  Compile("COMPILE");
  Compile(tok);
  Compile("RET");
}

function Macro(tok)
{
  # Compile("\"" tok "\"");
  Dict[MACRO,tok] = H;
  # printf "--- MACRO %d: %s\n",H,tok;
  Compile(tok);
  Compile("RET");
}

function InitDict()
{
  Prim("+");
  Prim("-");
  Prim("*");
  Prim("/");
  Prim("MOD");
  Prim("MIN");
  Prim("MAX");
  Prim("<");
  Prim(">");
  Prim("=");
  Prim("AND");
  Prim("OR");
  Prim("XOR");
  Prim("NEGATE");
  Prim("ABS");
  Prim("NOT");
  Prim("*/");
  Prim("DUP");
  Prim("DROP");
  Prim("SWAP");
  Prim("OVER");
  Prim("DECIMAL");
  Prim("HEX");
  Prim(".");
  Prim(".R");
  Prim("CR");
  Prim(":",1);
  Prim("VARIABLE");
  Prim("CREATE");
  Prim("ALLOT");
  Prim(",");
  Prim("@");
  Prim("!");
  Prim("EMPTY");
  Prim("BYE");

  Macro("IF");
  Macro("ELSE");
  Macro("THEN");
  Macro("FOR");
  Macro("I");
  Macro("NEXT");
  Macro(";");

  HMark = H;
  # printf "--- HMark = %d\n",HMark;
}

function DUP()      { S[++pS] = T; }
function DROP()     { T = S[pS--]; }
function THEN()     { M[T] = H; DROP(); }
function Compile(x) { M[H++] = x; }

function Head(nm)
{
  DollI++;
  nm = toupper($DollI);
  # printf "--- Head: %d %s\n",H,nm;
  Dict[D,nm] = H;
  DollI++;
}

function Create()   { Head(); Compile("DOVAR"); }
function Bool(x)    { return x ? -1 : 0; }

function Dot(val,width)
{
  if (val + 0 != val "") printf "%*s ",width,val;
  else if (HexOut) printf "%*x ",width,val;
  else printf "%*d ",width,val;
}

function Cycle(w)
{
  while(1) {
    w = M[P++];
    # printf "--- Cycle: %d %s\n",P-1,w;
    # C.H.Moore: Introductory vocabulary, same power as BASIC
    #  G.Haydon: Level-0 Forth
         if (w + 0 > 0)   { R[++pR] = I; I = P; P = w + 0; }
    else if (w == "@")    T = M[T];
    else if (w == "!")    { M[T] = S[pS--]; DROP(); }
    else if (w == "LIT")  { DUP(); T = M[P++]; }
    else if (w == "+")    T += S[pS--];
    else if (w == "-")    T = S[pS--] - T;
    else if (w == "*")    T *= S[pS--];
    else if (w == "/")    T = S[pS--] / T;
    else if (w == "MOD")  T = S[pS--] % T;
    else if (w == "MIN")  { w = T; DROP(); if (w < T) T = w; }
    else if (w == "MAX")  { w = T; DROP(); if (w > T) T = w; }
    else if (w == "<")    { T = Bool(S[pS--] <  T); }
    else if (w == ">")    { T = Bool(S[pS--] >  T); }
    else if (w == "=")    { T = Bool(S[pS--] == T); }
    else if (w == "AND")  { T = Bool(T && S[pS--]); }
    else if (w == "OR")   { T = Bool(T || S[pS--]); }
    else if (w == "XOR")  { w = S[pS--]; T = Bool((w && !T) || (!w && T)); }
    else if (w == "NEGATE") { T = -T; }
    else if (w == "ABS")  { if (T < 0) T = -T; }
    else if (w == "NOT")  { T = !T; }
    else if (w == "*/")   { w = T; DROP(); w *= T; DROP(); T = w / T; }
    else if (w == "DUP")  S[++pS] = T;
    else if (w == "DROP") T = S[pS--];
    else if (w == "SWAP") { w = T; T = S[pS]; S[pS] = w; }
    else if (w == "OVER") { w = S[pS]; DUP(); T = w; }
    else if (w == "POP")  { DUP(); T = I; I = R[pR--]; }
    else if (w == "PUSH") { R[++pR] = I; I = T; DROP(); }
    else if (w == "RAT")  { DUP(); T = I; }
    else if (w == "BRA")  P = M[P];
    else if (w == "BRZ")  { w = M[P++]; if (!T)    P = w; DROP(); }
    else if (w == "BRM")  { w = M[P++]; if (T < 0) P = w; DROP(); }
    else if (w == "DBRZ") { w = M[P++]; if (--I < 0) I = R[pR--]; else P = w; }
    else if (w == "RET")  { P = I; I = R[pR--]; }
    else if (w == "DECIMAL") { HexOut = 0; }
    else if (w == "HEX")  { HexOut = 1; }
    else if (w == ".")    { w = T; DROP(); Dot(w, 0); }
    else if (w == ".R")   { w = T; DROP(); Dot(T, w); DROP(); }
    else if (w == "CR")   { printf "\n"; }
    else if (w == ":")    { Head(); DoCompile(); }
    else if (w == ";")    { Compile("RET"); STATE = 0; }
    else if (w == "DOVAR")    { DUP(); T = H+1; P = I; I = R[pR--]; }
    else if (w == "VARIABLE") { Create(); H++; }
    else if (w == "CREATE")   { Create(); }
    else if (w == "ALLOT")    { H += T; DROP(); }
    else if (w == ",")        { Compile(T); DROP(); }
    else if (w == "EMPTY"){ H = HMark; }
    else if (w == "IF")   { Compile("BRZ"); DUP(); T = H; Compile(0); }
    else if (w == "ELSE") { Compile("BRA"); Compile(0); THEN(); DUP(); T=H-1; }
    else if (w == "THEN") { M[T] = H; DROP(); }
    else if (w == "FOR")  { Compile("PUSH"); DUP(); T = H; }
    else if (w == "I")    { Compile("RAT"); }
    else if (w == "NEXT") { Compile("DBRZ"); Compile(T); DROP(); }
    else if (w == "COMPILE") { Compile(M[P++]); }
    else if (w == "BYE")  { exit(0); }
    else if (w == "HALT") return;
  }
}

function Execute(p,oldXP,oldP)
{
  oldXP = XP;
  oldP  = P;
  P = XP;
  M[XP++] = p;
  M[XP++] = "HALT";
  Cycle();
  XP = oldXP;
   P = oldP;
}

# interpreter
function DoInterpret(i,w)
{
  while (1) {
    if (!NF) printf "OK ";
    else {
      for (; DollI <= NF; DollI++) {
        w = toupper($DollI);
        # printf "--- w: [%s]\n",w;
        if ((FORTH,w) in Dict) {
          Execute(Dict[FORTH,w]);
        } else if (match(w, /-?[0-9]+/)) {
          DUP(); T = w+0; 
        } else {
          printf "%s ? ",$DollI;
          DollI = NF+1;
          break;
        }
      }
    }
    Refill();
  }
}

# compiler
function DoCompile()
{
  # printf "--- DoCompile\n";
  STATE = 1;
  while (STATE) {
    for (; DollI <= NF; DollI++) {
      w = toupper($DollI);
      # printf "--- w: [%s]\n",w;
      if ((MACRO,w) in Dict) Execute(Dict[MACRO,w]);
      else if ((FORTH,w) in Dict) Compile(Dict[FORTH,w]);
      else if (match(w, /-?[0-9]+/)) {
        Compile("LIT");
        Compile(w + 0);
      } else {
        print "%s ? ",$DollI;
        STATE = 0; DollI = NF+1;
        break;
      }
    }
    if (STATE) Refill();
  }
}

END \
{
  for (i = 0; i < H; i++) {
    printf "%05d %s\n",i,M[i];
  }
  exit(0);
}
