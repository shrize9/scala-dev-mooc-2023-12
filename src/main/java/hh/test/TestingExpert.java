package hh.test;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;

/*
Вы работаете над модулем «Электронная зачетка» в системе для администрирования учебного процесса регионального вуза. Каждый студент может быть записан на несколько курсов, по каждому курсу у него есть итоговый балл.
Необходимо написать программу для учебного офиса. Программа будет получать и хранить списки студентов, а также итоговый балл студента по каждому курсу. Программа также должна уметь составлять отсортированный список студентов, которые набрали строго больше проходного балла по определенному предмету. Сортировка идет по убыванию баллов студентов за этот предмет.
Формат ввода
Первая строка содержит информацию о студентах, курсах и их оценках в формате: «имя_студента,курс,оценка;имя_студента,курс,оценка;...». В строке есть информация хотя бы об одном студенте. Вторая строка содержит предмет, по которому запрошена статистика, и его проходной балл: «курс,проходной_балл».
Все баллы являются целыми положительными числами, а все имена студентов уникальны.
Формат вывода
Имена студентов и их баллы за этот предмет через запятую без пробела, каждый студент с новой строки. Выводятся только те студенты, кто набрал строго больше проходного балла по этому предмету. Если студенты набрали одинаковый балл, они сортируются в порядке ввода.
Если никто не набрал проходной балл, выводится слово «Никто» (без кавычек).
Пример 1
Входные данные:
Анна,Математика,85;Анна,Химия,90;Борис,Математика,75;Борис,История,80;Евгений,Математика,95;Евгений,История,85
Математика,80
Выходные данные:
Евгений,95
Анна,85
Пример 2
Входные данные:
Анна,Психология,8;Алексей,Психология,6
Психология,8
Выходные данные:
Никто
* */
public class TestingExpert {

    class SaleTransaction {
        private String quart;
        private String name;
        private Integer amount;
        SaleTransaction(Integer quart, String name, Integer amount) {
            this.quart = "Q" +quart;
            this.name = name;
            this.amount = amount;
        }

        public String getQuart() {
            return quart;
        }

        public String getName() {
            return name;
        }

        public Integer getAmount() {
            return amount;
        }
    }
    public SaleTransaction parse(String line) throws ParseException {
        String[] parts = line.split(":");

        Integer quart = new SimpleDateFormat("yyyy-MM-dd").parse(parts[0].trim()).getMonth()/3+1;
        return new SaleTransaction(
                quart,
                parts[1].trim(),
                Integer.valueOf(parts[2].trim())
        );
    }

    public String generateReport(String salesData) {
        try {
            List<SaleTransaction> transactions =new LinkedList<>();
            for (String strTransaction : salesData.split(";")) {
                SaleTransaction saleTransaction = parse(strTransaction);
                transactions.stream().filter((t)-> t.getQuart().equals(saleTransaction.getQuart()) && t.getName().toLowerCase().equals(saleTransaction.getName().toLowerCase())).findFirst().ifPresentOrElse((saleTransaction1) -> {
                    saleTransaction1.amount += saleTransaction.getAmount();
                }, new Runnable() {
                    @Override
                    public void run() {
                        transactions.add(saleTransaction);
                    }
                });
            }
            Map<String, Set<SaleTransaction>> grouped=transactions.stream().collect(Collectors.groupingBy(SaleTransaction::getQuart, Collectors.toSet()));

            StringBuilder sb = new StringBuilder();
            for (Map.Entry<String, Set<SaleTransaction>> entry : grouped.entrySet()) {
                sb.append(entry.getKey()).append(":\n");
                for (SaleTransaction transaction : entry.getValue().stream().sorted((c,c1)->c.getName().compareTo(c1.getName())).collect(Collectors.toList())) {
                    sb.append("- ");
                    sb.append(transaction.getName() +": ");
                    sb.append(transaction.getAmount());
                    sb.append("\n");
                }
            }

            return sb.toString();
        }catch (Exception err){
            return err.getMessage();
        }
    }

    public String countDigits(String digits) {
        Map<Integer, Integer> result = new LinkedHashMap<>();

        for(Character c : digits.toCharArray()) {
            result.put(c - '0', result.getOrDefault(c - '0', 1)+1);
        }
        return result.entrySet().stream().map((entry)->"%d:%d".formatted(entry.getKey(), entry.getValue())).collect(Collectors.joining(","));
    }

    public static void main(String[] args) {
        TestingExpert testingExpert = new TestingExpert();

        System.out.println(testingExpert.generateReport("2023-02-05:Шляпа:4;2023-03-20:Кольцо:7;2023-04-25:Браслет:6;2023-04-26:Браслет:12"));
        System.out.println(testingExpert.generateReport("2023-03-05:Коврик:6;2023-04-25:Бинокль:10;2023-05-10:Компас:8;2023-03-05:Коврик:6;2023-04-25:Бинокль:10;2023-05-10:Компас:8"));
        System.out.println(testingExpert.generateReport("2023-05-20:Шапка:7;2023-02-25:Краска:5;2023-05-05:Мяч:8"));
    }
}
