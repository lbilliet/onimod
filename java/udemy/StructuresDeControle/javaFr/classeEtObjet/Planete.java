package classeEtObjet;

public class Planete {

	public String	nom;
	public String	matiere;
	public long		diametre;
	public int 	totalVisiteurs = 0;
	int nbNouveauxHumains=0;
	Atmosphere atmosphere ;
	Vaisseau vaisseau;

	int rotation(int angle) {
		int nbRotation = angle /360;
		System.out.println(nom + " a effectué " + nbRotation + " tours sur elle-même.");
		return nbRotation;
	}

	int revolution(int angle) {
		int nbRevolution = angle /360;
		System.out.println(nom + " a effectué " + nbRevolution + " tours autour de son étoile.");
		return nbRevolution ;
	}
	
	void accueillirVaisseau(int nbNouveauxHumains) {
		totalVisiteurs=totalVisiteurs+nbNouveauxHumains;
	}
	
	//surcharge méthode accueillirVaisseau via ajout d'un paramètre
	void accueillirVaisseau(String typeVaisseau) {
		if (typeVaisseau.equals("CHASSEUR")){
			nbNouveauxHumains=3;
			totalVisiteurs=totalVisiteurs+nbNouveauxHumains;
		}
		else if (typeVaisseau.equals("FREGATE")) {
			nbNouveauxHumains=12;
			totalVisiteurs=totalVisiteurs+nbNouveauxHumains;
		}
		else if (typeVaisseau.equals("CROISEUR")) {
			nbNouveauxHumains=50;
			totalVisiteurs=totalVisiteurs+nbNouveauxHumains;
		}
		else {
			System.out.println("Veuillez renseigner le type de vaisseau correctement via une de ces 3 options : CHASSEUR, FREGATE, CROISEUR");
		}
		
	}
	
	void afficherAtmo() {
		System.out.println("L'atmosphère de "+nom+" est composée : ");
        System.out.println("A "+atmosphere.txHydrogene+" % d'hydrogène");
        System.out.println("A "+atmosphere.txArgon+" % d'argon");
        System.out.println("A "+atmosphere.txCo2+" % de dioxyde de carbone");
        System.out.println("A "+atmosphere.txAzote+" % d'azote");
        System.out.println("A "+atmosphere.txHelium+" % d'hélium");
        System.out.println("A "+atmosphere.txMethane+" % de méthane");
        System.out.println("A "+atmosphere.txSodium+" % de sodium");
	}
	
	Vaisseau accueillirVaisseau(Vaisseau vaisseau) {
		return vaisseau;
	}
	
	void libérerBaie () {
		
	}
	
}
