unit TraceAstar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Sysutils,raylib,initVariable;

function Heuristic(a, b: TVector2): Integer;
procedure AStarPathfinding(startHex, endHex: Integer);

implementation
var
  //HexGrid: array[1..TotalHexes] of THexagon;
  StartHex, EndHex: Integer;
  PathFound: Boolean;
  Path: array of Integer;

// Heuristique de distance (hex distance)
function Heuristic(a, b: TVector2): Integer;
begin
  Result := Round(abs(a.x - b.x) + abs(a.y - b.y)) div 2;
end;



procedure AStarPathfinding(startHex, endHex: Integer);
var
  openList: array of Integer;
  closedList: array of Integer;
  current, neighbor, i, j: Integer;
  tempGCost, tempFCost: Integer;
  PathFound: Boolean;

  procedure AddToOpenList(hex: Integer);
  begin
    SetLength(openList, Length(openList) + 1);
    openList[High(openList)] := hex;
  end;
  procedure InitBlocHexagone;
   // indiquer les blocs, c est initialiste dans la fonction principal du deplacement
begin
   HexGrid[14].Poshexagone := Bloque;
   HexGrid[15].Poshexagone := Bloque;
   HexGrid[16].Poshexagone := Bloque;
   HexGrid[17].Poshexagone := Bloque;
   HexGrid[14].Color := red;
   HexGrid[15].Color := red;
   HexGrid[16].Color := red;
   HexGrid[17].Color := red;
   HexGrid[18].Poshexagone := Bloque;
   HexGrid[18].Color := red;
end;
  procedure RemoveFromOpenList(hex: Integer);
  var
    index, lastIndex: Integer;
  begin
    lastIndex := High(openList);
    for index := 0 to lastIndex do
    begin
      if openList[index] = hex then
      begin
        openList[index] := openList[lastIndex];
        SetLength(openList, Length(openList) - 1);
        Break;
      end;
    end;
  end;

  procedure AddToClosedList(hex: Integer);
  begin
    SetLength(closedList, Length(closedList) + 1);
    closedList[High(closedList)] := hex;
  end;

  function IsInClosedList(hex: Integer): Boolean;
  var
    k: Integer;
  begin
    Result := False;
    for k := 0 to High(closedList) do
    begin
      if closedList[k] = hex then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;

begin
  SetLength(openList, 0);
  SetLength(closedList, 0);
  PathFound := False;
  InitBlocHexagone;

  // Initialisation du point de départ
  HexGrid[startHex].GCost := 0;
  HexGrid[startHex].HCost := Heuristic(HexGrid[startHex].Center, HexGrid[endHex].Center);
  HexGrid[startHex].FCost := HexGrid[startHex].GCost + HexGrid[startHex].HCost;
  HexGrid[startHex].Open := True;

  AddToOpenList(startHex);

  // Boucle principale de l'algorithme A*
  while Length(openList) > 0 do
  begin
    // Trouver le noeud avec le plus petit F cost dans openList
    current := openList[0];
    for i := 1 to High(openList) do
    begin
      if HexGrid[openList[i]].FCost < HexGrid[current].FCost then
        current := openList[i];
    end;

    // Si on atteint la destination
    if current = endHex then
    begin
      PathFound := True;
      Break;
    end;

    // Retirer le noeud courant de la openList et l'ajouter à la closedList
    RemoveFromOpenList(current);
    AddToClosedList(current);
    HexGrid[current].Closed := True;

    // Examiner les voisins du noeud courant
    for j := 0 to 5 do
    begin
      neighbor := HexGrid[current].Neighbors[j];
      if (neighbor = 0) or (HexGrid[neighbor].Poshexagone = Bloque) or IsInClosedList(neighbor) then
        Continue;

      tempGCost := HexGrid[current].GCost + 1; // Chaque déplacement coûte 1

      if not HexGrid[neighbor].Open then
      begin
        HexGrid[neighbor].GCost := tempGCost;
        HexGrid[neighbor].HCost := Heuristic(HexGrid[neighbor].Center, HexGrid[endHex].Center);
        HexGrid[neighbor].FCost := HexGrid[neighbor].GCost + HexGrid[neighbor].HCost;
        HexGrid[neighbor].Parent := current;
        HexGrid[neighbor].Open := True;

        AddToOpenList(neighbor);
      end
      else if tempGCost < HexGrid[neighbor].GCost then
      begin
        HexGrid[neighbor].GCost := tempGCost;
        HexGrid[neighbor].FCost := HexGrid[neighbor].GCost + HexGrid[neighbor].HCost;
        HexGrid[neighbor].Parent := current;
      end;
    end;
  end;

  // Si un chemin a été trouvé
  if PathFound then
  begin
    SetLength(Path, 0);
    current := endHex;
    HexGrid[current].Color := YELLOW;

    // Backtracking pour reconstruire le chemin
    while current <> startHex do
    begin
      SetLength(Path, Length(Path) + 1);
      Path[High(Path)] := current;
      current := HexGrid[current].Parent;
      HexGrid[current].Color := YELLOW
    end;

    SetLength(Path, Length(Path) + 1);
    Path[High(Path)] := startHex;
    HexGrid[startHex].Color := YELLOW
  end;
end;
end.

