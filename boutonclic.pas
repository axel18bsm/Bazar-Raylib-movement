unit BoutonClic;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Sysutils,raylib;
const
ButtonWidth = 200;               //bouton sauvegarde
  ButtonHeight = 50;

  type
  TButtonAxel = record
    Rect: TRectangle;     // Rectangle du bouton
    NormalColor: TColor;  // Couleur normale
    HoverColor: TColor;   // Couleur lorsqu'on passe la souris dessus
    ClickedColor: TColor; // Couleur lorsqu'on clique
    IsClicked: Boolean;   // Indicateur de clic
  end;
  var
  ButtonSave: TButtonAxel;
  MousePosition: TVector2;
    ColorToUse:TColor;

 function CreateButton(X, Y, Width, Height: Single; NormalColor, HoverColor, ClickedColor: TColor): TButtonAxel;

implementation

 // Fonction pour créer un bouton
function CreateButton(X, Y, Width, Height: Single; NormalColor, HoverColor, ClickedColor: TColor): TButtonAxel;
begin
  Result.Rect := RectangleCreate(X, Y, Width, Height);
  Result.NormalColor := NormalColor;
  Result.HoverColor := HoverColor;
  Result.ClickedColor := ClickedColor;
  Result.IsClicked := False;
end;
end.

