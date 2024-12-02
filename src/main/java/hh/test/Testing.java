package hh.test;

import scala.util.matching.Regex;

import java.util.regex.Pattern;

public class Testing {

    public static String findDrops(String input) {
        if(input ==null) return "Нет";
        if(input =="") return "Нет";

        String[] splitted =input.split(" ");
        if(splitted.length <=1) return "Нет";

        StringBuilder result = new StringBuilder();
        for (int i = 1; i < splitted.length; i++) {
            if(Integer.valueOf(splitted[i-1]) - Integer.valueOf(splitted[i]) >=3){
                result.append(i).append(" ");
            }
        }

        if(result.isEmpty()){
            return "Нет";
        }

        return result.toString();
    }

    // Набор спецсимволов
    private static final String specialChars = "!@#$%^&*()-+";

    public static String findSafePasswords(String input) {
        if(input ==null) return "Не найдено";
        if(input =="") return "Не найдено";
        StringBuilder result = new StringBuilder();

        for(String currPassword: input.split(" ")){
            if(
                Pattern.compile("[" +specialChars +"]").matcher(currPassword).find() &
                Pattern.compile("\\W").matcher(currPassword).find() &
                Pattern.compile("\\w").matcher(currPassword).find() &
                Pattern.compile("\\d").matcher(currPassword).find()
            )
                result.append(currPassword).append(" ");
        }

        if(result.isEmpty()){
            return "Не найдено";
        }

        return result.toString().trim();
    }

    public static void main(String[] args) {
       System.out.println(findSafePasswords("Password1 Pass@word 12345 pass!word Passw@rd Password1!"));
       System.out.println(findSafePasswords("Password1 Pass@word 12345 pass!word"));
       System.out.println(findSafePasswords("yB3Fn9e^gGV% Ps3JQE(E-#*& YH^U#PlAiE&8 g0EzfMrHR+fb aAIBW$B0p@ua"));
    }
}
