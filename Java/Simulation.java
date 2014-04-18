import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Random;


public class Simulation {
	int GA_POPSIZE = 2048;
	double GA_ELITRATE = 0.10;
	double GA_MUTATIONRATE = 0.25;
	String GA_TARGET = "Hello world!";
	
	List<String> population = new ArrayList<String>();
	List<String> buffer = new ArrayList<String>();
	Random random = new Random();
	int z = 0;
	
	private String GenerateRandomString()
	{
		String s = "";
		for (int i = 0; i < GA_TARGET.length(); i++)
			s += (char)(((random.nextInt(100) + 1) % 90) + 32);
		return s;
	}
	
	private void InitializeAll()
	{
		for (int i = 0; i < GA_POPSIZE; i++)
		{
			population.add(GenerateRandomString());
			buffer.add(GenerateRandomString());
		}
	}
	
	private double fitness(String value)
	{
		double a = 0.0;
		for (int i = 0; i < GA_TARGET.length(); i++)
			a += Math.abs((int)value.charAt(i) - (int)GA_TARGET.charAt(i));
		return a;
	}
	
	private void mate()
	{
		int esize = (int)(GA_POPSIZE * GA_ELITRATE);  //esize = 205

		for (int i = 0; i < esize; i++)
			buffer.set(i, population.get(i));

		for (int i = esize; i < GA_POPSIZE; i++) //205 to 2048
		{
			int i1 = random.nextInt(GA_POPSIZE / 2);
			int i2 = random.nextInt(GA_POPSIZE / 2);
			int spos = random.nextInt(GA_TARGET.length());
			buffer.set(i, population.get(i1).substring(0, spos) + population.get(i2).substring(spos, GA_TARGET.length())); //mate

			if (random.nextDouble() < GA_MUTATIONRATE)
			{
				int pos = random.nextInt( GA_TARGET.length()-1);
				buffer.set(i, buffer.get(i).substring(0,pos) + (char)random.nextInt(122) + buffer.get(i).substring(pos+1, GA_TARGET.length())); //insert random middle char
			}
		}
	}
	
	private void swap()
	{
		ArrayList<String> temp = new ArrayList<String>();
		for(String s : population)
			temp.add(s);

		for (int i = 0; i < buffer.size(); i++)
			population.set(i, buffer.get(i));
		
		for (int i = 0; i < temp.size(); i++)
			buffer.set(i, temp.get(i));
	}
	
	public Simulation()
	{
		InitializeAll();
		int i = 0;
		while (true)
		{
			Collections.sort(population, new Comparator<String>() {
		        public int compare(String s1, String s2) {
		            int i1 = (int) fitness(s1);
		            int i2 = (int) fitness(s2);
		            return Integer.compare(i1, i2);
		        }
		    });	
			System.out.println("("+fitness(population.get(0))+")" + "\t" + population.get(0) );
			if (fitness(population.get(0)) == 0) 
                break;
			mate();
			swap();
		}
	}
}
