package structuresDeControle;

public class SwitchCase {
	public static void main(String... args) {

		int nbPlanetes = 8;

		switch (nbPlanetes) {
		case 8:
			System.out.println("Aux derni�res nouvelles, le nombre total de plan�tes dans le syst�me solaire est de : " + nbPlanetes);
			break;
		case 9:
			System.out.println("Il y a quelques ann�es cependant, elles �taient au nombre de : " + nbPlanetes);
			break;
		case 10:
			System.out.println("Au cours de l'�re moderne, le nombre de plan�tes n'a jamais �t� officiellement de : " + nbPlanetes);
			break;

		}

		nbPlanetes++;

		switch (nbPlanetes) {
		case 8:
			System.out.println("Aux derni�res nouvelles, le nombre total de plan�tes dans le syst�me solaire est de : " + nbPlanetes);
			break;
		case 9:
			System.out.println("Il y a quelques ann�es cependant, elles �taient au nombre de : " + nbPlanetes);
			break;
		case 10:
			System.out.println("Au cours de l'�re moderne, le nombre de plan�tes n'a jamais �t� officiellement de : " + nbPlanetes);
			break;

		}

		nbPlanetes++;

		switch (nbPlanetes) {
		case 8:
			System.out.println("Aux derni�res nouvelles, le nombre total de plan�tes dans le syst�me solaire est de : " + nbPlanetes);
			break;
		case 9:
			System.out.println("Il y a quelques ann�es cependant, elles �taient au nombre de : " + nbPlanetes);
			break;
		case 10:
			System.out.println("Au cours de l'�re moderne, le nombre de plan�tes n'a jamais �t� officiellement de : " + nbPlanetes);
			break;

		}

	}

}
