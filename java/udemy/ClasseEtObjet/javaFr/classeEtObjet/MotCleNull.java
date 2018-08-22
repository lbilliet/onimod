package classeEtObjet;

public class MotCleNull {

	public static void main(String... args) {
		Planete mercure = new Planete();
		mercure.nom = "Mercure";
		mercure.diametre = 4880;
		mercure.matiere = "Tellurique";
		Planete venus = new Planete();
		venus.nom = "Venus";
		venus.diametre = 12100;
		venus.matiere = "Tellurique";
		Planete terre = new Planete();
		terre.nom = "Terre";
		terre.diametre = 12756;
		terre.matiere = "Tellurique";
		Planete mars = new Planete();
		mars.nom = "Mars";
		mars.diametre = 6792;
		mars.matiere = "Tellurique";
		Planete jupiter = new Planete();
		jupiter.nom = "Jupiter";
		jupiter.diametre = 142984;
		jupiter.matiere = "Gazeuse";
		Planete saturne = new Planete();
		saturne.nom = "Saturne";
		saturne.diametre = 120536;
		saturne.matiere = "Gazeuse";
		Planete uranus = new Planete();
		uranus.nom = "Uranus";
		uranus.diametre = 51118;
		uranus.matiere = "Gazeuse";
		Planete neptune = new Planete();
		neptune.nom = "Neptune";
		neptune.diametre = 49532;
		neptune.matiere = "Gazeuse";
		Planete sedna = new Planete();
		sedna.nom = null;
		sedna.diametre = 0;
		sedna.matiere = null;

		System.out.println(jupiter.nom + " est une planète " + jupiter.matiere + " avec un diamètre de "
				+ jupiter.diametre + " kilomètres.");
		System.out.println(sedna.nom + " est une planète " + sedna.matiere + " avec un diamètre de "
				+ sedna.diametre + " kilomètres.");

	}

}
