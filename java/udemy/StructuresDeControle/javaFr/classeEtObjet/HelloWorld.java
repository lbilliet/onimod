package classeEtObjet;

public class HelloWorld {
	public static void main(String... args) {

		Planete mercure = new Planete();
		mercure.nom = "Mercure";
		mercure.matiere = "Tellurique";
		mercure.diametre = 4880;
		Planete venus = new Planete();
		venus.nom = "Venus";
		venus.matiere = "Tellurique";
		venus.diametre = 12100;
		Planete terre = new Planete();
		terre.nom = "Terre";
		terre.matiere = "Tellurique";
		terre.diametre = 12756;
		Planete jupiter = new Planete();
		jupiter.nom = "Jupiter";
		jupiter.matiere = "gazeuse";
		jupiter.diametre = 142984;
		System.out
				.println(jupiter.nom + " est une planète " + jupiter.matiere + " avec un diamètre de " + jupiter.diametre + " kilometres");
	}

}
