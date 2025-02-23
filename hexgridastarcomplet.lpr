program hexgridastarcomplet;

uses
  Math,SysUtils, BoutonClic,raylib, TraceAstar, initVariable;




  // Calcule les 6 sommets d'un hexagone "tête pointue"
function PairOuImpair(Number: Integer):boolean;
begin
  if (Number mod 2 = 0) then
    PairOuImpair:=true // pair
  else
    PairOuImpair:=false;
end;


procedure PositionHexagone();
   var a:TPoint;
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
 function ColorToString(Color: TColor): string;
begin
  Result := Format('%d,%d,%d', [Color.r, Color.g, Color.b]);
end;

// Fonction pour convertir TEmplacement en chaîne de caractères
function EmplacementToString(Emplacement: TEmplacement): string;
begin
  case Emplacement of
    inconnu: Result := 'inconnu';
    CoinHG: Result := 'CoinHG';
    CoinHD: Result := 'CoinHD';
    CoinBG: Result := 'CoinBG';
    CoinBD: Result := 'CoinBD';
    BordH: Result := 'BordH';
    BordB: Result := 'BordB';
    BordG: Result := 'BordG';
    BordD: Result := 'BordD';
    Classic: Result := 'Classic';
  end;
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

        ////if (i + j) mod 2 = 0 then
        ////begin                                                                   // permet de mettre une couleur en decalage
        //  HexGrid[hexNumber].Color := GREEN;
        //end
        //else
        //begin
        //  HexGrid[hexNumber].Color := LIGHTGRAY;
        //End;

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

  procedure SaveHexGridToCSV();
  var
    F: TextFile;
    i, k: Integer;
    NeighborStr, VerticesStr: string;
  begin
    AssignFile(F, SaveFileName);
    Rewrite(F);
    try
      // Ecrire l'en-tête du CSV
      Writeln(F, 'Number,CenterX,CenterY,ColorR,ColorG,ColorB,Selected,Colonne,Ligne,Emplacement,PairImpairLigne,' +
                 'Vertex1X,Vertex1Y,Vertex2X,Vertex2Y,Vertex3X,Vertex3Y,Vertex4X,Vertex4Y,Vertex5X,Vertex5Y,Vertex6X,Vertex6Y,' +
                 'Neighbor1,Neighbor2,Neighbor3,Neighbor4,Neighbor5,Neighbor6');

      for i := 1 to TotalNbreHex do
      begin
        // Sauvegarder les voisins comme une chaîne séparée par des virgules
        NeighborStr := Format('%d,%.,%d,%d,%d,%d', [HexGrid[i].Neighbors[1], HexGrid[i].Neighbors[2],
                                                    HexGrid[i].Neighbors[3], HexGrid[i].Neighbors[4],
                                                    HexGrid[i].Neighbors[5], HexGrid[i].Neighbors[6]]);

        // Sauvegarder les vertices comme une chaîne séparée par des virgules
        VerticesStr := '';
        for k := 0 to 5 do
        begin
          VerticesStr := VerticesStr + Format('%d,%d', [HexGrid[i].Vertices[k].x, HexGrid[i].Vertices[k].y]);
          if k < 5 then
            VerticesStr := VerticesStr + ',';  // Ajouter une virgule sauf après le dernier point
        end;

        // Sauvegarder toutes les informations de l'hexagone
        Writeln(F, Format('%d,%.0f,%.0f,%d,%d,%d,%s,%d,%d,%s,%s,%s,%s',                   //%d,%d, // HexGrid[i].Center.x, HexGrid[i].Center.y,
          [HexGrid[i].Number,
           HexGrid[i].Center.x, HexGrid[i].Center.y,
           HexGrid[i].Color.r, HexGrid[i].Color.g, HexGrid[i].Color.b,
           BoolToStr(HexGrid[i].Selected, True),
           HexGrid[i].Colonne, HexGrid[i].Ligne,
           EmplacementToString(HexGrid[i].Poshexagone),
           BoolToStr(HexGrid[i].PairImpairLigne, True),
           VerticesStr,  // Ajout des vertices
           NeighborStr]));
      end;
    finally
      CloseFile(F);
    end;
  end;

  procedure SaveHexGridToFile();
var
  F: TextFile;
  i: Integer;
  NeighborStr: string;

begin
  AssignFile(F, SaveFileName);
  Rewrite(F);
  try
    for i := 1 to TotalNbreHex do
    begin
      // Sauvegarder les voisins comme une chaîne séparée par des virgules
      NeighborStr := Format('%d,%d,%d,%d', [HexGrid[i].Neighbors[1], HexGrid[i].Neighbors[2],
                                                  HexGrid[i].Neighbors[3], HexGrid[i].Neighbors[4],
                                                  HexGrid[i].Neighbors[5], HexGrid[i].Neighbors[6]]);
      // Sauvegarder toutes les informations de l'hexagone
      Writeln(F, Format('Hexagone #%d:', [HexGrid[i].Number]));
     Writeln(F, Format('  Centre: (%.0f, %.0f)', [HexGrid[i].Center.x, HexGrid[i].Center.y]));
      Writeln(F, Format('  Couleur: %s', [ColorToString(HexGrid[i].Color)]));
      Writeln(F, Format('  Colonne: %d', [HexGrid[i].Colonne]));
      Writeln(F, Format('  Ligne: %d', [HexGrid[i].Ligne]));
      Writeln(F, Format('  Emplacement: %s', [EmplacementToString(HexGrid[i].Poshexagone)]));
      Writeln(F, Format('  Pair/Impair Ligne: %s', [BoolToStr(HexGrid[i].PairImpairLigne, True)]));
      Writeln(F, Format('  Voisins: %s', [NeighborStr]));                        // %d,%d,
      Writeln(F, '');  // Ligne vide pour la séparation
    end;
  finally
    CloseFile(F);
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
      MousePosition:=Vector2Create(mouseX,mouseY);
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
      // bouton normaux
      { #todo : inserer un bouton pour sauvegarder }
         if CheckCollisionPointRec(MousePosition, ButtonSave.Rect) then

          begin
          ButtonSave.IsClicked := True;
       { #todo : appeler la procedure de sauvegarde } // Changer la couleur de fond
       //SaveHexGridToFile();
       SaveHexGridToCSV;
    end;
    // Changer la couleur du bouton en mode "hover"
    ColorToUse := ButtonSave.HoverColor;
  end
  else
  begin
    ColorToUse := ButtonSave.NormalColor;
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
     // affiche le boutton de sauvegaarde:
     DrawRectangleRec(Buttonsave.Rect, ColorToUse);
     DrawText('Sauve la grille', Round(Buttonsave.Rect.x+25), Round(Buttonsave.Rect.y + 15), 20, BLACK);
    if HexSelected then
    begin
      InfoText := Format('Numéro de l''hexagone: %d'#10 +
        'Point Central: (%.0f, %.0f)'#10 +
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
  ButtonSave := CreateButton(550, 500, ButtonWidth, ButtonHeight, DARKBLUE, SKYBLUE, RED);
                                                                              // maintenant, il est possible de calculer les voisins.
  if faitAstar =true then AStarPathfinding(3,30);                             //test pour lancement calcul
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
