program hexpointokpointu13end;

uses
  raylib,
  Math,
  SysUtils;

const
  HexDiameter = 60;              // Diamètre de chaque hexagone
  HexRadius = HexDiameter / 2;    // Rayon de chaque hexagone
  HexHeight = HexDiameter;        // Hauteur de l'hexagone (tête pointue)
  HexWidth = HexRadius * sqrt(3); // Largeur totale d'un hexagone (distance entre deux sommets opposés)
  decalageRayon =2;               // la detection se fait par la longueur du rayon, indiquez un nombre pour ne pas prendre 2 cases jointes.
  columns = 5;                    // Nombre d'hexagones en largeur
  rows = 8;                       // Nombre d'hexagones en hauteur
  InfoBoxWidth = 300;             // Largeur du cadre d'informations
  InfoBoxHeight = 150;            // Hauteur de la zone d'informations
  WindowWidth = 800;              // Largeur de la fenêtre
  WindowHeight = 600 + InfoBoxHeight; // Hauteur de la fenêtre (inclut la zone d'information)
  TotalNbreHex=Columns * Rows;      // nombre hexagons
  CoinIn=false;                     // extremement important, si celui est false alors case 1 = extremegauche, sinon decalé par rapport à la 2eme ligne

type
  TPoint = record
    x, y: integer;
  end;

  TEmplacement = (inconnu, CoinHG, CoinHD, CoinBG, CoinBD, BordH, BordB,BordG,BordD,Classic);

  // Structure d'un hexagone avec un numéro, centre, couleur, sélection et voisins
  THexCell = record
    Number: integer;              // Numéro de l'hexagone (de 1 à 30)
    Center: TPoint;               // Point central de l'hexagone
    Vertices: array[0..5] of TPoint;  // Les 6 sommets de l'hexagone
    Color: TColor;                // Couleur de l'hexagone
    Selected: boolean;            // État de sélection par clic
    Neighbors: array[1..6] of integer;  // Numéros des voisins contigus (6 voisins)
    Colonne: integer;                 // quelle est la colonne de cet haxagone
    ligne: integer;                   // quelle est la ligne de cet hexagone
    Poshexagone:TEmplacement;         // l'emplacement va nous servir pour connaitre la strategie à adopter pour trouver les voisins
    PairImpairLigne:boolean;           // va nous servir pour trouver les voisins
  end;

var
  HexGrid: array[1..TotalNbreHex] of THexCell;                  // colonne et ligne drive la table
  //HexGridNumber: array[1..TotalNbreHex] of THexCell;                // le numero d hexagone drive la table
  i, j,k: integer;
  SelectedHex: THexCell;  // Hexagone actuellement sélectionné
  HexSelected: boolean;   // Indique si un hexagone est sélectionné


  // Calcule les 6 sommets d'un hexagone "tête pointue"
function PairOuImpair(Number: Integer):boolean;
begin
  if (Number mod 2 = 0) then
    PairOuImpair:=true // pair
  else
    PairOuImpair:=false;
end;
procedure PositionHexagone();

 begin
   for I := 1 to TotalNbreHex do HexGrid[i].Poshexagone:=inconnu;       //initialisation de tous les  champs à inconnu(0)

   for I := 1 to totalnbrehex do
   begin
     if HexGrid[i].Colonne=1 then HexGrid[i].Poshexagone:=BordG;           //bord gauche car 1ere colonne
     if HexGrid[i].ligne=1 then HexGrid[i].Poshexagone:=BordH;             //bordhaut  car 1ere ligne
     if HexGrid[i].colonne=columns then HexGrid[i].Poshexagone:=BordD;     //borddroit car derniere clonne
     if HexGrid[i].ligne=rows then HexGrid[i].Poshexagone:=Bordb;        //bordbas derniere ligne car on le connait à l initialisation
   end;

   // les coins                                                                //on ecrase les coins
     HexGrid[1].Poshexagone:=CoinHG;                                    // le 1 c'est haut gauche
     HexGrid[TotalNbreHex].Poshexagone:=CoinBD;                         // le dernier, c'est bas droit.
     HexGrid[columns].Poshexagone:=CoinHD;                              // coin haut droit
     HexGrid[TotalNbreHex-columns+1].Poshexagone:=CoinBG;                // coin bas gauche

   // lereste des inconnus sont des classic
      for I := 1 to totalnbrehex do
   begin
     if HexGrid[i].Poshexagone=inconnu then HexGrid[i].Poshexagone:=classic;           //inconnu en classic
     end;
End;
procedure TrouveLesVoisins();
//TEmplacement = (inconnu, CoinHG, CoinHD, CoinBG, CoinBD, BordH, BordB,BordG,BordD,Classic);

begin

        If CoinIn =False Then                                                    // separation pour etre plus lisible
        begin
           for I := 1 to TotalNbreHex  do
           begin
                case HexGrid[i].Poshexagone of
                 inconnu:
                   begin
                     for j := 1 to 6 do
                     begin
                       HexGrid[i].Neighbors[j]:=0;                              //hexagone inconnu donc pas de voisin
                     end;
                   end;
                 CoinHG:
                   begin                                                        // toujours ligne impaire
                     HexGrid[i].Neighbors[1]:=0;
                     HexGrid[i].Neighbors[2]:=HexGrid[i].Number+1;
                     HexGrid[i].Neighbors[3]:=HexGrid[i].Number+columns;
                     HexGrid[i].Neighbors[4]:=0;
                     HexGrid[i].Neighbors[5]:=0;
                     HexGrid[i].Neighbors[6]:=0;
                   end;
                 CoinHD:                                                        // toujours ligne impaire
                   begin
                     HexGrid[i].Neighbors[1]:=0;
                     HexGrid[i].Neighbors[2]:=0;
                     HexGrid[i].Neighbors[3]:=HexGrid[i].Number+columns;
                     HexGrid[i].Neighbors[4]:=HexGrid[i].Number+columns-1;
                     HexGrid[i].Neighbors[5]:=HexGrid[i].Number-1;
                     HexGrid[i].Neighbors[6]:=0;
                   end;
                 Coinbg:
                   if HexGrid[i].PairImpairLigne=false then                      //impair
                   begin
                     HexGrid[i].Neighbors[1]:=HexGrid[i].Number-columns;
                     HexGrid[i].Neighbors[2]:=HexGrid[i].Number+1;
                     HexGrid[i].Neighbors[3]:=0;
                     HexGrid[i].Neighbors[4]:=0;
                     HexGrid[i].Neighbors[5]:=0;
                     HexGrid[i].Neighbors[6]:=0;
                   end
                   else
                   begin
                     HexGrid[i].Neighbors[1]:=HexGrid[i].Number-columns+1;      //pair
                     HexGrid[i].Neighbors[2]:=HexGrid[i].Number+1;
                     HexGrid[i].Neighbors[3]:=0;
                     HexGrid[i].Neighbors[4]:=0;
                     HexGrid[i].Neighbors[5]:=0;
                     HexGrid[i].Neighbors[6]:=HexGrid[i].Number-columns;
                   end;
                 Coinbd:
                  if HexGrid[i].PairImpairLigne=false then                      //ligne impair
                   begin
                     HexGrid[i].Neighbors[1]:=HexGrid[i].Number-columns;
                     HexGrid[i].Neighbors[2]:=0;
                     HexGrid[i].Neighbors[3]:=0;
                     HexGrid[i].Neighbors[4]:=0;
                     HexGrid[i].Neighbors[5]:=HexGrid[i].Number-1;
                     HexGrid[i].Neighbors[6]:=HexGrid[i].Number-columns-1;
                   end
                   else
                   begin
                     HexGrid[i].Neighbors[1]:=0;                                 //pair
                     HexGrid[i].Neighbors[2]:=0;
                     HexGrid[i].Neighbors[3]:=0;
                     HexGrid[i].Neighbors[4]:=0;
                     HexGrid[i].Neighbors[5]:=HexGrid[i].Number-1;
                     HexGrid[i].Neighbors[6]:=HexGrid[i].Number-columns;
                   end;
                 BordH:                                                         //toujours impair
                 begin
                     HexGrid[i].Neighbors[1]:=0;
                     HexGrid[i].Neighbors[2]:=HexGrid[i].Number+1;
                     HexGrid[i].Neighbors[3]:=HexGrid[i].Number+columns;
                     HexGrid[i].Neighbors[4]:=HexGrid[i].Number+columns-1;
                     HexGrid[i].Neighbors[5]:=HexGrid[i].Number-1;
                     HexGrid[i].Neighbors[6]:=0;
                   end;
                 BordB:
                 if HexGrid[i].PairImpairLigne=false then                        //ligne impaire
                 begin
                     HexGrid[i].Neighbors[1]:=HexGrid[i].Number-columns;
                     HexGrid[i].Neighbors[2]:=HexGrid[i].Number+1;
                     HexGrid[i].Neighbors[3]:=0;
                     HexGrid[i].Neighbors[4]:=0;
                     HexGrid[i].Neighbors[5]:=HexGrid[i].Number-1;
                     HexGrid[i].Neighbors[6]:=HexGrid[i].Number-columns-1;
                   end
                   else
                   begin
                     HexGrid[i].Neighbors[1]:=HexGrid[i].Number-columns+1;      //ligne paire
                     HexGrid[i].Neighbors[2]:=HexGrid[i].Number+1;
                     HexGrid[i].Neighbors[3]:=0;
                     HexGrid[i].Neighbors[4]:=0;
                     HexGrid[i].Neighbors[5]:=HexGrid[i].Number-1;
                     HexGrid[i].Neighbors[6]:=HexGrid[i].Number-columns;
                   end;
                 BordG:
                 begin
                     if HexGrid[i].PairImpairLigne=false then                     //ligne impaire
                     begin
                     HexGrid[i].Neighbors[1]:=HexGrid[i].Number-columns;
                     HexGrid[i].Neighbors[2]:=HexGrid[i].Number+1;
                     HexGrid[i].Neighbors[3]:=HexGrid[i].Number+columns;
                     HexGrid[i].Neighbors[4]:=0;
                     HexGrid[i].Neighbors[5]:=0;
                     HexGrid[i].Neighbors[6]:=0;
                     end
                     else
                     begin
                     HexGrid[i].Neighbors[1]:=HexGrid[i].Number-columns+1;      //ligne paire
                     HexGrid[i].Neighbors[2]:=HexGrid[i].Number+1;
                     HexGrid[i].Neighbors[3]:=HexGrid[i].Number+columns+1;
                     HexGrid[i].Neighbors[4]:=HexGrid[i].Number+columns;
                     HexGrid[i].Neighbors[5]:=0;
                     HexGrid[i].Neighbors[6]:=HexGrid[i].Number-columns;
                     end;
                     end;
                 BordD:
                     begin
                    if HexGrid[i].PairImpairLigne=false then                    //ligne impaire
                    begin
                    HexGrid[i].Neighbors[1]:=HexGrid[i].Number-columns;
                    HexGrid[i].Neighbors[2]:=0;
                    HexGrid[i].Neighbors[3]:=HexGrid[i].Number+columns;
                    HexGrid[i].Neighbors[4]:=HexGrid[i].Number+columns-1;
                    HexGrid[i].Neighbors[5]:=HexGrid[i].Number-1;
                    HexGrid[i].Neighbors[6]:=HexGrid[i].Number-columns-1;
                    end
                    else
                    begin
                    HexGrid[i].Neighbors[1]:=0;                                 //ligne paire
                    HexGrid[i].Neighbors[2]:=0;
                    HexGrid[i].Neighbors[3]:=0;
                    HexGrid[i].Neighbors[4]:=HexGrid[i].Number+columns;
                    HexGrid[i].Neighbors[5]:=HexGrid[i].Number-1;
                    HexGrid[i].Neighbors[6]:=HexGrid[i].Number-columns;
                    end;
                    end;
                     Classic:
                     begin
                    if HexGrid[i].PairImpairLigne=false then                    //ligne impaire
                    begin
                    HexGrid[i].Neighbors[1]:=HexGrid[i].Number-columns;
                    HexGrid[i].Neighbors[2]:=HexGrid[i].Number+1;
                    HexGrid[i].Neighbors[3]:=HexGrid[i].Number+columns;
                    HexGrid[i].Neighbors[4]:=HexGrid[i].Number+columns-1;
                    HexGrid[i].Neighbors[5]:=HexGrid[i].Number-1;
                    HexGrid[i].Neighbors[6]:=HexGrid[i].Number-columns-1;
                    end
                    else
                    begin
                    HexGrid[i].Neighbors[1]:=HexGrid[i].Number-columns+1;       //ligne paire
                    HexGrid[i].Neighbors[2]:=HexGrid[i].Number+1;
                    HexGrid[i].Neighbors[3]:=HexGrid[i].Number+columns+1;
                    HexGrid[i].Neighbors[4]:=HexGrid[i].Number+columns;
                    HexGrid[i].Neighbors[5]:=HexGrid[i].Number-1;
                    HexGrid[i].Neighbors[6]:=HexGrid[i].Number-columns;
                    end;
                    end;
                   end;
                end;
           end;



        If CoinIn =True Then
        begin
           for I := 1 to TotalNbreHex  do
           begin
                case HexGrid[i].Poshexagone of
                 inconnu:
                   begin
                     for j := 1 to 6 do
                     begin
                       HexGrid[i].Neighbors[j]:=0;                              //hexagone inconnu donc pas de voisin
                     end;
                   end;
                 CoinHG:
                   begin                                                        // toujours ligne impaire
                     HexGrid[i].Neighbors[1]:=0;
                     HexGrid[i].Neighbors[2]:=HexGrid[i].Number+1;
                     HexGrid[i].Neighbors[3]:=HexGrid[i].Number+columns+1;
                     HexGrid[i].Neighbors[4]:=HexGrid[i].Number+columns;
                     HexGrid[i].Neighbors[5]:=0;
                     HexGrid[i].Neighbors[6]:=0;
                   end;
                 CoinHD:                                                        // toujours ligne impaire
                   begin
                     HexGrid[i].Neighbors[1]:=0;
                     HexGrid[i].Neighbors[2]:=0;
                     HexGrid[i].Neighbors[3]:=0;
                     HexGrid[i].Neighbors[4]:=HexGrid[i].Number+columns;
                     HexGrid[i].Neighbors[5]:=HexGrid[i].Number-1;
                     HexGrid[i].Neighbors[6]:=0;
                   end;
                 Coinbg:
                   if HexGrid[i].PairImpairLigne=false then                      //impair
                   begin
                     HexGrid[i].Neighbors[1]:=HexGrid[i].Number-columns+1;
                     HexGrid[i].Neighbors[2]:=HexGrid[i].Number+1;
                     HexGrid[i].Neighbors[3]:=0;
                     HexGrid[i].Neighbors[4]:=0;
                     HexGrid[i].Neighbors[5]:=0;
                     HexGrid[i].Neighbors[6]:=HexGrid[i].Number;
                   end
                   else
                   begin
                     HexGrid[i].Neighbors[1]:=HexGrid[i].Number-columns;      //pair
                     HexGrid[i].Neighbors[2]:=HexGrid[i].Number+1;
                     HexGrid[i].Neighbors[3]:=0;
                     HexGrid[i].Neighbors[4]:=0;
                     HexGrid[i].Neighbors[5]:=0;
                     HexGrid[i].Neighbors[6]:=0;
                   end;
                 Coinbd:
                  if HexGrid[i].PairImpairLigne=false then                      //ligne impair
                   begin
                     HexGrid[i].Neighbors[1]:=0;
                     HexGrid[i].Neighbors[2]:=0;
                     HexGrid[i].Neighbors[3]:=0;
                     HexGrid[i].Neighbors[4]:=0;
                     HexGrid[i].Neighbors[5]:=HexGrid[i].Number-1;
                     HexGrid[i].Neighbors[6]:=HexGrid[i].Number-columns;
                   end
                   else
                   begin
                     HexGrid[i].Neighbors[1]:=HexGrid[i].Number-columns;                                 //pair
                     HexGrid[i].Neighbors[2]:=0;
                     HexGrid[i].Neighbors[3]:=0;
                     HexGrid[i].Neighbors[4]:=0;
                     HexGrid[i].Neighbors[5]:=HexGrid[i].Number-1;
                     HexGrid[i].Neighbors[6]:=HexGrid[i].Number-columns-1;
                   end;
                 BordH:                                                         //toujours impair
                 begin
                     HexGrid[i].Neighbors[1]:=0;
                     HexGrid[i].Neighbors[2]:=HexGrid[i].Number+1;
                     HexGrid[i].Neighbors[3]:=HexGrid[i].Number+columns+1;
                     HexGrid[i].Neighbors[4]:=HexGrid[i].Number+columns;
                     HexGrid[i].Neighbors[5]:=HexGrid[i].Number-1;
                     HexGrid[i].Neighbors[6]:=0;
                   end;
                 BordB:
                 if HexGrid[i].PairImpairLigne=false then                        //ligne impaire
                 begin
                     HexGrid[i].Neighbors[1]:=HexGrid[i].Number-columns+1;
                     HexGrid[i].Neighbors[2]:=HexGrid[i].Number+1;
                     HexGrid[i].Neighbors[3]:=0;
                     HexGrid[i].Neighbors[4]:=0;
                     HexGrid[i].Neighbors[5]:=HexGrid[i].Number-1;
                     HexGrid[i].Neighbors[6]:=HexGrid[i].Number-columns;
                   end
                   else
                   begin
                     HexGrid[i].Neighbors[1]:=HexGrid[i].Number-columns;      //ligne paire
                     HexGrid[i].Neighbors[2]:=HexGrid[i].Number+1;
                     HexGrid[i].Neighbors[3]:=0;
                     HexGrid[i].Neighbors[4]:=0;
                     HexGrid[i].Neighbors[5]:=HexGrid[i].Number-1;
                     HexGrid[i].Neighbors[6]:=HexGrid[i].Number-columns-1;
                   end;
                 BordG:
                 begin
                     if HexGrid[i].PairImpairLigne=false then                     //ligne impaire
                     begin
                     HexGrid[i].Neighbors[1]:=HexGrid[i].Number-columns+1;
                     HexGrid[i].Neighbors[2]:=HexGrid[i].Number+1;
                     HexGrid[i].Neighbors[3]:=HexGrid[i].Number+columns+1;
                     HexGrid[i].Neighbors[4]:=HexGrid[i].Number+columns;
                     HexGrid[i].Neighbors[5]:=0;
                     HexGrid[i].Neighbors[6]:=HexGrid[i].Number-columns;
                     end
                     else
                     begin
                     HexGrid[i].Neighbors[1]:=HexGrid[i].Number-columns;      //ligne paire
                     HexGrid[i].Neighbors[2]:=HexGrid[i].Number+1;
                     HexGrid[i].Neighbors[3]:=HexGrid[i].Number+columns;
                     HexGrid[i].Neighbors[4]:=0;
                     HexGrid[i].Neighbors[5]:=0;
                     HexGrid[i].Neighbors[6]:=0;
                     end;
                     end;
                 BordD:
                     begin
                    if HexGrid[i].PairImpairLigne=false then                    //ligne impaire
                    begin
                    HexGrid[i].Neighbors[1]:=0;
                    HexGrid[i].Neighbors[2]:=0;
                    HexGrid[i].Neighbors[3]:=0;
                    HexGrid[i].Neighbors[4]:=HexGrid[i].Number+columns;
                    HexGrid[i].Neighbors[5]:=HexGrid[i].Number-1;
                    HexGrid[i].Neighbors[6]:=HexGrid[i].Number-columns;
                    end
                    else
                    begin
                    HexGrid[i].Neighbors[1]:=HexGrid[i].Number-columns;                                 //ligne paire
                    HexGrid[i].Neighbors[2]:=0;
                    HexGrid[i].Neighbors[3]:=HexGrid[i].Number+columns;
                    HexGrid[i].Neighbors[4]:=HexGrid[i].Number+columns-1;
                    HexGrid[i].Neighbors[5]:=HexGrid[i].Number-1;
                    HexGrid[i].Neighbors[6]:=HexGrid[i].Number-columns-1;
                    end;
                    end;
                     Classic:
                     begin
                    if HexGrid[i].PairImpairLigne=false then                    //ligne impaire
                    begin
                    HexGrid[i].Neighbors[1]:=HexGrid[i].Number-columns+1;
                    HexGrid[i].Neighbors[2]:=HexGrid[i].Number+1;
                    HexGrid[i].Neighbors[3]:=HexGrid[i].Number+columns+1;
                    HexGrid[i].Neighbors[4]:=HexGrid[i].Number+columns;
                    HexGrid[i].Neighbors[5]:=HexGrid[i].Number-1;
                    HexGrid[i].Neighbors[6]:=HexGrid[i].Number-columns;
                    end
                    else
                    begin
                    HexGrid[i].Neighbors[1]:=HexGrid[i].Number-columns;       //ligne paire
                    HexGrid[i].Neighbors[2]:=HexGrid[i].Number+1;
                    HexGrid[i].Neighbors[3]:=HexGrid[i].Number+columns;
                    HexGrid[i].Neighbors[4]:=HexGrid[i].Number+columns-1;
                    HexGrid[i].Neighbors[5]:=HexGrid[i].Number-1;
                    HexGrid[i].Neighbors[6]:=HexGrid[i].Number-columns-1;
                    end;
                    end;
                   end;
                end;
           end
end;

procedure CalculateHexVertices(var Hex: THexCell);
// permet de creer un hexagon.
  var
    angle_deg, angle_rad: single;
    k: integer;
  begin
    for k := 0 to 5 do
    begin
      angle_deg := 30 + 60 * k;  // Angle de rotation pour un hexagone "tête pointue"
      angle_rad := PI / 180 * angle_deg;
      Hex.Vertices[k].x := Round(Hex.Center.x + HexRadius * cos(angle_rad));
      Hex.Vertices[k].y := Round(Hex.Center.y + HexRadius * sin(angle_rad));
    end;
  end;
procedure CalculateNeighbors();
begin
   //' il faut trouver les lignes pairs et impairs, c'est fait dans l initialisation
   //' il faut caler les types d emplacement
         PositionHexagone();
   //' il faut ensuite  positionner les voisins.les voisins, c est un mixte quelle ligne et de coinIn
   // qui vont determiner les voisins d'un autre pour les tetes pointues pour les tetes plates c'est
   // encore different
         TrouveLesVoisins();
end;


  // Initialise la grille hexagonale avec des numéros, couleurs et centre, et ligne colonnes.
  procedure InitializeHexGrid();
  var
    offsetX, offsetY: single;
    hexNumber:Integer;
  begin
    hexNumber := 1;  // Initialisation du compteur d'hexagones
    for j := 1 to rows do
    begin
      for i := 1 to columns do
      begin
        offsetX := i * HexWidth;  // Décalage horizontal
        offsetY := j * (HexHeight * 0.75);// Décalage vertical avec 3/4 de la hauteur d'un hexagone

        // Extremement important : Décalage pour créer la disposition en "nid d'abeille"
        // si decalage negatif, la 1ere case de la 1ere ligne sera à l'extreme gauche, sinon cela
        //sera la 1ere case de la seconde ligne qui le sera  et cela changera la position des voisins. La position des voisins est sensible
        //à cette variation, surtout pour toute hexagone sur un bord.
        if (j mod 2) = 1 then
        begin
         if  coinIn=false then offsetX := offsetX - (HexWidth / 2)      // pas propre
         else
         offsetX := offsetX + (HexWidth / 2)     //pas propre
        end;

        // Définition du centre de l'hexagone
        HexGrid[hexnumber].Center.x := Round(HexRadius + offsetX);              // l hexagone sera positionné sur le centre de l hexagone
        HexGrid[hexnumber].Center.y := Round(HexHeight / 2 + offsetY);          // donc un vecteur (x,y) est calculé et stocké dans la structure hexagone

        // Définit le numéro de l'hexagone
        HexGrid[hexNumber].Number := hexNumber;                                 // onstocke le numero de l hexagone
        HexGrid[hexNumber].colonne := i;   //colonne                            // son numero de colonne
        HexGrid[hexNumber].ligne := j;    //rows                                //son numero de ligne
        HexGrid[hexNumber].PairImpairLigne:=PairOuImpair(HexGrid[hexNumber].ligne);            // la ligne paire ou impaire va nous servir à connaitre les voisins

        if (i + j) mod 2 = 0 then
        begin                                                                   // permet de mettre une couleur en decalage
          HexGrid[hexNumber].Color := GREEN;
        end
        else
        begin
          HexGrid[hexNumber].Color := LIGHTGRAY;
        End;

        HexGrid[hexnumber].Selected := False;                                   // toujours à faux au demarrage, personne n a cliqué


        CalculateHexVertices(HexGrid[hexnumber]);                               // Calcule les sommets de l'hexagone

      Inc(hexNumber);
      end;
    //CalculateNeighbors();  // Calcule les voisins contigus de chaque hexagone
    end;
end;

  // Dessine la grille hexagonale avec les numéros d'hexagones au centre
  procedure DrawHexGrid(dessineLesNombres:boolean);
  var

    hexNumberText: array[0..5] of char;
    outlineColor: TColor;

  begin

    for i := 1 to TotalNbreHex do                                                // numéro de l hexagone

      begin
        DrawPoly(Vector2Create(HexGrid[i].Center.x, HexGrid[i].Center.y ),       //on pourrait ameliorer le code en remplacant les
          6, HexRadius - 1, 30, HexGrid[i].Color);                               //tpoints en tvector2. Pascal est strict.

        if HexGrid[i].Selected then
          outlineColor := ORANGE
        else
          outlineColor := DARKGRAY;

        DrawPolyLinesEx(Vector2Create(HexGrid[I].Center.x, HexGrid[I].Center.y),
          6, HexRadius, 30, 2, outlineColor);
       if dessineLesNombres=True then
         begin
        StrPCopy(hexNumberText, IntToStr(HexGrid[I].Number));
        DrawText(hexNumberText, Round(HexGrid[I].Center.x - 10),
          Round(HexGrid[I].Center.y - 10), 20, BLACK);
         End;
         end;
      end;





  // Gère la détection de clic sur un hexagone et met à jour les informations
  procedure HandleMouseClick();
  var
    mouseX, mouseY: integer;
    dx, dy: single;
    dist: single;
  begin
    if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) then
    begin
      mouseX := GetMouseX();
      mouseY := GetMouseY();
      HexSelected := False;

      for i := 1 to TotalNbreHex do

        begin
          dx := mouseX - HexGrid[i].Center.x;
          dy := mouseY - HexGrid[i].Center.y;
          dist := sqrt(dx * dx + dy * dy);
          if dist <= HexRadius-decalageRayon then  //diminution du rayon entre -1 et -2 pour ne pas cliquer sur 2 hexagones joints
          begin                                    // plus le rayon est long, plus le decalage doit etre important.
            HexGrid[i].Selected := True;
            SelectedHex := HexGrid[i];
            HexSelected := True;
          end
          else
            HexGrid[i].Selected := False;
        end;
      end;
    end;


  // Affiche les informations de l'hexagone sélectionné
  procedure DrawHexInfoBox();
  var
    InfoText: string;
  begin
    DrawRectangle(0, WindowHeight - InfoBoxHeight, WindowWidth, InfoBoxHeight, LIGHTGRAY);
    // Cadre de fond
    DrawRectangleLines(0, WindowHeight - InfoBoxHeight, WindowWidth,
      InfoBoxHeight, DARKGRAY); // Contour

    if HexSelected then
    begin
      InfoText := Format('Numéro de l''hexagone: %d'#10 +
        'Point Central: (%d, %d)'#10 +
        'Couleur: %s'#10 +
        'Voisins: %d, %d, %d, %d, %d, %d'#10 +
        'ligne : %d' + ' colonne : %d'#10+
        'Position : %d',
        [SelectedHex.Number, SelectedHex.Center.x,
        SelectedHex.Center.y, 'Vert',
        SelectedHex.Neighbors[1], SelectedHex.Neighbors[2],
        SelectedHex.Neighbors[3], SelectedHex.Neighbors[4],
        SelectedHex.Neighbors[5], SelectedHex.Neighbors[6], SelectedHex.ligne,
        SelectedHex.Colonne,SelectedHex.Poshexagone]);
      DrawText(PChar(InfoText), 20, WindowHeight - InfoBoxHeight + 20, 20, BLACK);
    end
    else
      DrawText('Aucun hexagone sélectionné.', 20, WindowHeight -
        InfoBoxHeight + 20, 20, BLACK);
  end;

begin
  InitWindow(WindowWidth, WindowHeight, 'Hexagonal Grid - Pointed Top with Neighbors');
  SetTargetFPS(60);
  InitializeHexGrid();
  CalculateNeighbors();                                                         //initialisation des hexagones
                                                                                // maintenant, il est possible de calculer les voisins.
  while not WindowShouldClose() do
  begin
    HandleMouseClick();

    BeginDrawing();
    ClearBackground(RAYWHITE);
    DrawHexGrid(true);  // true dessine les nombres de heagones, false ne le fais pas
    DrawHexInfoBox();
    EndDrawing();
  end;

  CloseWindow();
end.
