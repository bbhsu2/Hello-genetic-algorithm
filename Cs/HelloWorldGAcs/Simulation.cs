using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HelloWorldGAcs
{
	public class Simulation
	{
		readonly int GA_POPSIZE = 2048;
		readonly double GA_ELITRATE = 0.10;
		readonly double GA_MUTATIONRATE = 0.25;
		readonly string GA_TARGET = "Hello world!";

		List<string> population = new List<string>();
		List<string> buffer = new List<string>();
		Random random = new Random();

		private string GenerateRandomString()
		{
			string s = null;
			for (int i = 0; i < random.Next(GA_TARGET.Length, GA_TARGET.Length); i++)
				s += Convert.ToChar((random.Next(1,100) % 90) + 32);
			return s;
		}

		private void InitializeAll()
		{
			for (int i = 0; i < GA_POPSIZE; i++)
			{
				population.Add(GenerateRandomString());
				buffer.Add(GenerateRandomString());
			}
		}

		private double fitness(string value)
		{
			double a = 0.0;

			for (int i = 0; i < value.Length; i++)
				a += Math.Abs((int)value[i] - (int)GA_TARGET[i]);

			return a;
		}

		private void mate()
		{
			int esize = (int)(GA_POPSIZE * GA_ELITRATE);  //esize = 205

			for (int i = 0; i < esize; i++)
				buffer[i] = population[i];

			for (int i = esize; i < GA_POPSIZE; i++) //205 to 2048
			{
				int i1 = random.Next(0, GA_POPSIZE / 2);
				int i2 = random.Next(0, GA_POPSIZE / 2);
				int spos = random.Next(0, GA_TARGET.Length);
				buffer[i] = population[i1].Substring(0, spos) + population[i2].Substring(spos, population[i2].Length - spos); //mate

				if (random.NextDouble() < GA_MUTATIONRATE)
				{
					int pos = random.Next(0, GA_TARGET.Length-1);
					buffer[i] = buffer[i].Substring(0, pos) + (char)random.Next(0,122)+ buffer[i].Substring(pos+1, buffer[i].Length - pos-1); //insert random middle char
				}
			}
		}

		private void swap()
		{
			List<string> temp = new List<string>();
			foreach (string s in population)
				temp.Add(s);

			for (int i = 0; i < buffer.Count; i++)
				population[i] = buffer[i];
			
			for (int i = 0; i < temp.Count; i++)
				buffer[i] = temp[i];
		}

		public Simulation()
		{
			InitializeAll();

			while (true)
			{
				population.Sort ( (x, y) => fitness (x).CompareTo (fitness (y)));
				Console.WriteLine("Best ({0})\t{1}", fitness(population[0]), population[0]);
				if (fitness(population[0]) == 0) 
                    break;
				mate();
				swap();
			}
		}
	}
}