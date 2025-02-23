program hexpointokmodelenepastoucher4;

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
  columns = 6;                    // Nombre d'hexagones en largeur
  rows = 11;                       // Nombre d'hexagones en hauteur
  InfoBoxWidth = 300;             // Largeur du cadre d'informations
  InfoBoxHeight = 150;            // Hauteur de la zone d'informations
  WindowWidth = 800;              // Largeur de la fenêtre
  WindowHeight = 600 + InfoBoxHeight; // Hauteur de la fenêtre (inclut la zone d'information)
  TotalNbreHex=Columns * Rows;      // nombre hexagons


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
  HexGrid: array[1..columns, 1..rows] of THexCell;                  // colonne et ligne drive la table
  HexGridNumber: array[1..TotalNbreHex] of THexCell;                // le numero d hexagone drive la table
  i, j,k: integer;
  SelectedHex: THexCell;  // Hexagone actuellement sélectionné
  HexSelected: boolean;   // Indique si un hexagone est sélectionné
  decalageNegatif:boolean;  // extremement important, si celui est true alor case 1 = extremegauche

  // Calcule les 6 sommets d'un hexagone "tête pointue"
function PairOuImpair(Number: Integer):boolean;
begin
  if (Number mod 2 = 0) then
    PairOuImpair:=true // pair
  else
    PairOuImpair:=false;
end;
procedure PositionHexagone();
  var numhexa:integer;
 begin
   for I := 1 to TotalNbreHex do HexGridNumber[i].Poshexagone:=inconnu;       //initialisation du champ

   for I := 1 to totalnbrehex do
   begin
     if HexGridNumber[i].Colonne=1 then HexGridNumber[i].Poshexagone:=BordG;           //bord gauche
     if HexGridNumber[i].ligne=1 then HexGridNumber[i].Poshexagone:=BordH;             //bordhaut
     if HexGridNumber[i].colonne=columns then HexGridNumber[i].Poshexagone:=BordD;     //borddroit
     if HexGridNumber[i].colonne=rows then HexGridNumber[i].Poshexagone:=Bordb;        //bordbas
   end;

   // les coins                                                                //on ecrase les coins
     HexGridNumber[1].Poshexagone:=CoinHG;                                    // le 1 c'est haut gauche
     HexGridNumber[TotalNbreHex].Poshexagone:=CoinBD;                         // le dernier, c'est bas droit.
     HexGridNumber[columns].Poshexagone:=CoinHD;                              // coin haut droit
     HexGridNumber[TotalNbreHex-columns+1].Poshexagone:=CoinBD;                // coin bas gauche

   // lereste des inconnus sont des classic
      for I := 1 to totalnbrehex do
   begin
     if HexGridNumber[i].Poshexagone=inconnu then HexGridNumber[i].Poshexagone:=classic;           //inconnu en classic
             //bordbas
   end;
      for j := 1 to rows do
      begin
           for i := 1 to columns do
           begin
           numhexa:=HexGrid[j][i].number;
           HexGrid[j][i].Poshexagone:=HexGridNumber[numhexa].Poshexagone;
           end;
      end;
End;




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
   //' il faut ensuite  positionner les voisins.

end;


  // Initialise la grille hexagonale avec des numéros, couleurs et centre, et ligne colonnes.
  procedure InitializeHexGrid();
  var
    offsetX, offsetY: single;
    hexNumber:Integer;
  begin
    decalageNegatif:=true;
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
         if  decalageNegatif=true then offsetX := offsetX - (HexWidth / 2);      // pas propre
         if  decalageNegatif=false then offsetX := offsetX + (HexWidth / 2);     //pas propre
        end;




        // Définition du centre de l'hexagone
        HexGrid[i][j].Center.x := Round(HexRadius + offsetX);
        HexGrid[i][j].Center.y := Round(HexHeight / 2 + offsetY);
        HexGridNumber[hexNumber].Center.x :=HexGrid[i][j].Center.x;
        HexGridnumber[hexNumber].Center.y :=HexGrid[i][j].Center.y;

        // Définit le numéro de l'hexagone
        HexGrid[i][j].Number := hexNumber;
        HexGridnumber[hexNumber].Number:=HexGrid[i][j].Number;
        HexGrid[i][j].colonne := i;   //colonne
        HexGridnumber[hexNumber].colonne:=HexGrid[i][j].colonne;
        HexGrid[i][j].ligne := j;    //rows
        HexGridnumber[hexNumber].ligne:=HexGrid[i][j].ligne;
        HexGrid[i][j].PairImpairLigne:=PairOuImpair(hexNumber);
        HexGridnumber[hexNumber].PairImpairLigne:=HexGrid[i][j].PairImpairLigne;

        if (i + j) mod 2 = 0 then
          HexGrid[i][j].Color := GREEN

        else
          HexGrid[i][j].Color := LIGHTGRAY;
          hexGridnumber[hexNumber].Color:=HexGrid[i][j].Color;                   // ne pas deplacer ce code
        HexGrid[i][j].Selected := False;
        hexGridnumber[hexNumber].Selected:=HexGrid[i][j].Selected;

        CalculateHexVertices(HexGrid[i][j]); // Calcule les sommets de l'hexagone
        begin
    for k := 0 to 5 do
    begin
     hexGridnumber[hexNumber].Vertices[k].x:= HexGrid[i][j].Vertices[k].x;
     hexGridnumber[hexNumber].Vertices[k].y:= HexGrid[i][j].Vertices[k].y;
    end;

    end;
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
    for i := 1 to columns do
    begin
      for j := 1 to rows do
      begin
        DrawPoly(Vector2Create(HexGrid[i][j].Center.x, HexGrid[i][j].Center.y),
          6, HexRadius - 1, 30, HexGrid[i][j].Color);

        if HexGrid[i][j].Selected then
          outlineColor := ORANGE
        else
          outlineColor := DARKGRAY;

        DrawPolyLinesEx(Vector2Create(HexGrid[i][j].Center.x, HexGrid[i][j].Center.y),
          6, HexRadius, 30, 2, outlineColor);
       if dessineLesNombres=True then
         begin
        StrPCopy(hexNumberText, IntToStr(HexGrid[i][j].Number));
        DrawText(hexNumberText, Round(HexGrid[i][j].Center.x - 10),
          Round(HexGrid[i][j].Center.y - 10), 20, BLACK);
         End;
         end;
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

      for i := 1 to columns do
      begin
        for j := 1 to rows do
        begin
          dx := mouseX - HexGrid[i][j].Center.x;
          dy := mouseY - HexGrid[i][j].Center.y;
          dist := sqrt(dx * dx + dy * dy);
          if dist <= HexRadius-decalageRayon then  //diminution du rayon entre -1 et -2 pour ne pas cliquer sur 2 hexagones joints
          begin
            HexGrid[i][j].Selected := True;
            SelectedHex := HexGrid[i][j];
            HexSelected := True;
          end
          else
            HexGrid[i][j].Selected := False;
        end;
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
  CalculateNeighbors();

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
