package structuresDeControle;

public class HelloUniverse1 {

	public static void main(String... args) {

		int nbPlanetes = 8;

		if (nbPlanetes == 8) {
			System.out.println("Aux dernières nouvelles, le nombre total de planètes dans le système solaire est de : " + nbPlanetes);
		}

		else {
			System.out.println("Il y a quelques années cependant, elles étaient au nombre de : " + nbPlanetes);
		}

		nbPlanetes++;

		if (nbPlanetes == 8) {
			System.out.println("Aux dernières nouvelles, le nombre total de planètes dans le système solaire est de : " + nbPlanetes);
		}

		else {
			System.out.println("Il y a quelques années cependant, elles étaient au nombre de : " + nbPlanetes);
		}

	}
}
