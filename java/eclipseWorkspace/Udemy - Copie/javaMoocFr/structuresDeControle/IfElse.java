package structuresDeControle;

public class IfElse {
	
    public static void main(String... args) {
        String phrase = "Aux derni�res nouvelles, le nombre total de plan�tes dans le syst�me solaire est de : ";
        int nbPlanetes = 8;
        if (nbPlanetes == 8) {
            System.out.println(phrase + nbPlanetes);
        }
        phrase = "Il y a quelques ann�es cependant, elles �taient au nombre de : ";
        if (nbPlanetes == 9) {
            System.out.println(phrase + nbPlanetes);
        }

    }

}
