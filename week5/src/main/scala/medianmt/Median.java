package medianmt;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.Comparator;
import java.util.PriorityQueue;
import java.util.Scanner;

/**
 * Created by Dandoh on 11/16/16.
 */
public class Median {

    public static void main(String[] args) {
        int MODULO = 10000;

        Scanner scanner = null;
        try {
            scanner = new Scanner(new FileInputStream("Median.txt"));


            // begin
            PriorityQueue<Integer> upper = new PriorityQueue<>();
            PriorityQueue<Integer> lower = new PriorityQueue<>(new Comparator<Integer>() {
                @Override
                public int compare(Integer o1, Integer o2) {
                    return -o1.compareTo(o2);
                }
            });
            upper.add(Integer.MAX_VALUE);
            lower.add(Integer.MIN_VALUE);

            int sum = 0;
            while (scanner.hasNextInt()) {
                int x = scanner.nextInt();

                if (x > upper.peek()) {
                    upper.add(x);
                } else {
                    lower.add(x);
                }

                // maintain
                if (lower.size() > upper.size() + 1) {
                    upper.add(lower.poll());
                }

                if (upper.size() > lower.size() + 1) {
                    lower.add(upper.poll());
                }

                if (upper.size() > lower.size()) {
                    sum += upper.peek();
                    sum %= MODULO;
                } else {
                    sum += lower.peek();
                    sum %= MODULO;
                }


            }

            System.out.println(sum);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }

    }
}
