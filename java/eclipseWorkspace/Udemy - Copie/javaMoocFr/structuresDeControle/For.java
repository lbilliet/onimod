package structuresDeControle;

public class For {

	public static void main(String[] args) {

		for (int nbPlanetes = 8; nbPlanetes < 11; nbPlanetes++) {
			switch (nbPlanetes) {
			case 8:
				System.out.println("Aux dernières nouvelles, le nombre total de planètes dans le système solaire est de : " + nbPlanetes);
				break;
			case 9:
				System.out.println("Il y a quelques années cependant, elles étaient au nombre de : " + nbPlanetes);
				break;
			default:
				System.out.println("Au cours de l'ère moderne, le nombre de planètes n'a jamais été officiellement de : " + nbPlanetes);
			}
		}
	}
}
