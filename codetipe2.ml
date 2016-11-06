open Unix;;
open Graphics;;
#load "graphics.cma";;
#load "unix.cma";;

type image = {im : int array array ; filtre : float ; exposition : int};;

let remplir s = let m=Array.make_matrix 512 512 0 in
		let tab = Array.make 4 0 in
		let buffer = String.make  1 '0' in
		let fichier = Unix.openfile s [O_RDONLY] 0o640 in
for i=0 to 14
 do
Unix.read fichier buffer 0 1;
done;    
while (Unix.read fichier buffer 0 1)=1 
do
if ((48<=int_of_char buffer.[0])&&(int_of_char buffer.[0]<=57))
then 
  begin
      tab.(2)<-1;
      if tab.(3)=0 
      then
         m.(tab.(0)).(tab.(1))<-(10*m.(tab.(0)).(tab.(1))+(int_of_char buffer.[0]-48))
  end
else
  if tab.(2)=1
  then
   begin
   tab.(3)<-(tab.(3)+1) mod 3;
   if tab.(3)=0 then    
	  if tab.(1)<511 
          then tab.(1)<-(tab.(1)+1)
          else
             begin tab.(1)<-0; tab.(0)<-(tab.(0)+1);
             end;
    tab.(2)<-0;
    end
   
done;
m;;

let affiche_matrice m =
  let largeur = Array.length m in
  let longueur = Array.length m.(0) in
  let value = ref 0 in
  Graphics.open_graph " 600x512";
  for i=0 to largeur -1 do
    for j=0 to longueur -1 do
      value := m.(i).(j);
      set_color (rgb !value !value !value);
      plot i j;
    done;
  done;
  let sortie=ref true in
  while(!sortie) do
    let a = Graphics.wait_next_event [Graphics.Key_pressed;Graphics.Mouse_motion] in
    if a.keypressed then sortie:= false
    else
      begin 
	set_color (rgb 255 255 255);
	fill_rect 512 0 600 512;
	set_color (rgb 0 0 0);
	moveto 520 225;
	draw_string (string_of_int a.mouse_x); 
	moveto 550 225;
	draw_string (string_of_int a.mouse_y);
      end
  done;
  Graphics.close_graph ();;


let shift m i j =
(*i correspond à l'abscisse et j à l'ordonnée, on ordonne comme dans le plan euclidien*)
  let n=Array.length m in
  let res = Array.make_matrix n n 0 in
  for k=0 to n-1 do
    for l=0 to n-1 do
      if ((k+j>=0)&&(l-i>=0)&&(k+j<n)&&(l-i<n)) then res.(k).(l)<- m.(k+j).(-i+l);
    done;
  done;
  res;;

let difference m1 (box1,box2) (box3,box4) m2 (shiftx, shifty) =
(* si shiftx>0 on va à droite dans la matrice, si shifty>0 on monte dans la matrice*)
  let res = ref 0 in
  for i=box3 to box4 do
    for j=box1 to box2 do
      res:= !res + max (m1.(i).(j)-m2.(i-shifty).(j+shiftx)) (m2.(i-shifty).(j+shiftx)-m1.(i).(j));
    done;
  done;
!res;;

let rechercher_position m1 (box1,box2) (box3,box4) m2 (shiftx, shifty) =
  let resx = ref (shiftx)in
  let resy= ref (shifty) in
  for s=0 to 2*(box4-box3) do
    for t=0 to 2*(box2-box1) do
      print_int 0;
      let d=difference m1 (box1,box2) (box3,box4) m2 (t+shiftx-(box2-box1),s+shifty-(box4-box3)) in
      if d < difference m1 (box1,box2) (box3,box4) m2 (!resx, !resy) then (resx:=t ; resy:=s)
    done;
  done;
(!resx, !resy);;

let aligner im1 im2 (box1,box2) (box3,box4) (shiftx,shifty) =
  let (x,y)=rechercher_position im1.im (box1,box2) (box3,box4) im2.im (shiftx,shifty) in
  let m= shift im2.im x y in
({im=im1.im ; filtre = im1.filtre ; exposition=im1.exposition},{im=m ; filtre=im2.filtre ; exposition=im2.exposition});;

let combiner im1 im2 = 
  let exp= ref 0 in
  let m=Array.make_matrix 512 512 0. in
  for i=0 to 511 do
    for j=0 to 511 do
      if (((im2.im.(i).(j)-100)<im1.im.(i).(j))&&(im1.im.(i).(j)<(im2.im.(i).(j)+100)))
      then m.(i).(j)<-float_of_int(im1.im.(i).(j)+im2.im.(i).(j))/.float_of_int(im1.exposition +im2.exposition)
      else
	begin
	if im1.im.(i).(j)<im2.im.(i).(j)
	then  exp := im1.exposition
        else  exp := im2.exposition;
	     m.(i).(j)<-float_of_int(min im1.im.(i).(j) im2.im.(i).(j))/.float_of_int( !exp)

	end
    done;
  done;
  m;;

let mean_value m (box1,box2) (box3,box4) =
  let somme= ref 0. and element = ref 0 in
for i=box3 to box4 do
for j=box1 to box2 do
somme := !somme +. m.(i).(j);
element := !element +1
done;
done;
!somme/.(float_of_int !element);;

let soustraction m v =
  let longueur = Array.length m in
  let largeur = Array.length m.(0) in
  for i=0 to longueur-1 do
    for j=0 to largeur-1 do
m.(i).(j)<-m.(i).(j)-v
    done;
  done;
m;;

let rechercher_max mat =
let longueur = Array.length mat in
let largeur = Array.length mat.(0) in
let max = ref 0. in
for i=0 to longueur-1 do
  for j=0 to largeur-1 do
    if mat.(i).(j)> !max then max:=mat.(i).(j)
  done;
done;
!max;;

let multiplier m k = 
  let longueur = Array.length m in
  let largeur = Array.length m.(0) in
  for i=0 to longueur-1 do
    for j=0 to largeur-1 do
      m.(i).(j)<-k*.m.(i).(j)
    done;
  done;
m;;

let intmatrix_of_floatmatrix m = 
  let longueur = Array.length m in
  let largeur = Array.length m.(0) in
  let mat = Array.make_matrix longueur largeur 0 in
  for i=0 to longueur -1 do
    for j=0 to largeur -1 do
      mat.(i).(j)<-int_of_float m.(i).(j)
    done;
  done;
mat;;

let recalibrer pic filtre =
  let mat = multiplier pic filtre in
  let max = rechercher_max mat in
  let c=254./.max in
  let m = multiplier mat c in
intmatrix_of_floatmatrix m;;


let fabriquer_image m resul= 
  let fichier = Unix.openfile ("/home/juliette/"^resul) [O_CREAT;O_WRONLY] 0o640 in
  ignore(write fichier "P2 512 512 255 \n" 0 16);
  let rec aux n i j = 
try Unix.write fichier (string_of_int m.(i).(j)) 0 n with _->aux (n-1) i j
in
  for i=0 to 511 do
    for j=0 to 511 do
     ignore(aux 3 i j);
       ignore(Unix.write fichier "\n" 0 1)
    done;
  done;;

let matrice1 = remplir "/home/juliette/Images/n315ct1_converti.ppm";;
let matrice2 = remplir "/home/juliette/Images/n315ct2_converti.ppm";;
let im1 = {im=matrice1 ; filtre=7.6 ; exposition = 200};;
let im2= {im=matrice2 ; filtre=7.6 ; exposition = 160};;
let (image1,image2)=aligner im1 im2 (281,300) (276,292) (0,0);;
affiche_matrice image2.im;;
let pic=combiner image1 image2;;
let m=recalibrer pic 7.6;;
affiche_matrice m;;
fabriquer_image m "resul";;
