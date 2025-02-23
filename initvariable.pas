unit initVariable;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Sysutils,raylib;
const
  HexDiameter = 60;              // Diamètre de chaque hexagone
  HexRadius = HexDiameter / 2;    // Rayon de chaque hexagone
  HexHeight = sqrt(3) * hexRadius;  // Hauteur de l'hexagone (tête pointue)
  HexWidth = 2 * hexRadius; // Largeur totale d'un hexagone (distance entre deux sommets opposés)
  decalageRayon =2;               // la detection se fait par la longueur du rayon, indiquez un nombre pour ne pas prendre 2 cases jointes.
  columns = 7;                    // Nombre d'hexagones en largeur
  rows = 8;                       // Nombre d'hexagones en hauteur
  InfoBoxWidth = 300;             // Largeur du cadre d'informations
  InfoBoxHeight = 150;            // Hauteur de la zone d'informations
  WindowWidth = 800;              // Largeur de la fenêtre
  WindowHeight = 600 + InfoBoxHeight; // Hauteur de la fenêtre (inclut la zone d'information)
  TotalNbreHex=Columns * Rows;      // nombre hexagons
  CoinIn=false;                     // extremement important, si celui est false alors case 1 = extremegauche, sinon decalé par rapport à la 2eme ligne
  SaveFileName = 'hexgridplat.csv'; //  nom sauvegarde fichier
  faitAstar=True;                   // declenche le calcul cheminement

type
  TPoint = record
    x, y: integer;
  end;

  TEmplacement = (inconnu, CoinHG, CoinHD, CoinBG, CoinBD, BordH, BordB,BordG,BordD,Classic,Bloque);

  // Structure d'un hexagone avec un numéro, centre, couleur, sélection et voisins
  THexCell = record
    Number: integer;              // Numéro de l'hexagone
    center: Tvector2;               // Point central de l'hexagone
    Vertices: array[0..5] of TPoint;  // Les 6 sommets de l'hexagone
    Color: TColor;                // Couleur de l'hexagone
    Selected: boolean;            // État de sélection par clic
    Neighbors: array[1..6] of integer;  // Numéros des voisins contigus (6 voisins)
    Colonne: integer;                 // quelle est la colonne de cet haxagone,rows
    ligne: integer;                   // quelle est la ligne de cet hexagone, lines
    Poshexagone:TEmplacement;         // l'emplacement va nous servir pour connaitre la strategie à adopter pour trouver les voisins
    PairImpairLigne:boolean;           // va nous servir pour trouver les voisins tete pointue
    GCost, HCost, FCost: Integer;  // permet d effectuer les couts du trajet A*
    Parent: Integer;               // un parent possible, toujours pour A*
    Closed: Boolean;               // il est traité par A*
    Open: Boolean;                 //il est en cours de traitement par A*
    PairImpaircolonne:boolean;       //va nous servir pour trouver les voisins tete plate
  end;
  ////THexagon = record
  // // Number: Integer;
  //  Center: TVector2;
  //  Color: TColor;
  //  Parent: Integer;
  //  Neighbors: array[0..5] of Integer;
  //  Poshexagone: TEmplacement;
  //  Closed: Boolean;
  //  Open: Boolean;
  //  Selected: Boolean;
  //end;
  type
  TButtonAxel = record
    Rect: TRectangle;     // Rectangle du bouton
    NormalColor: TColor;  // Couleur normale
    HoverColor: TColor;   // Couleur lorsqu'on passe la souris dessus
    ClickedColor: TColor; // Couleur lorsqu'on clique
    IsClicked: Boolean;   // Indicateur de clic
  end;
var
  HexGrid: array[1..TotalNbreHex] of THexCell;                          // le numero d hexagone drive la table
  //HexGridNumber: array[1..TotalNbreHex] of THexCell;                // le numero d hexagone drive la table
  i, j,k: integer;
  SelectedHex: THexCell;  // Hexagone actuellement sélectionné
  HexSelected: boolean;   // Indique si un hexagone est sélectionné
  Path: array of Integer;// le cheminastar

implementation

end.

