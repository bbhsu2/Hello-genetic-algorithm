/* This version written by Bernard Hsu on Saturday 11/15/2014 */

#include <iostream>
#include <vector>
#define GA_POPSIZE 2048
#define GA_ELITRATE 0.10
#define GA_MUTATIONRATE 25
#define GA_TARGET std::string("Hello world!")

std::vector<std::string> population;
std::vector<std::string> buffer(GA_POPSIZE);

std::string GenerationRandomString()
{
	std::string s = "";
	for (int i = 0; i < GA_TARGET.length(); ++i)
		s += rand() % 90 + 32;
	return s;
}

void InitializeAll()
{
	for (int i = 0; i < GA_POPSIZE; i++)
		population.push_back(GenerationRandomString());
}

int fitness(std::string const& value)
{
	int a = 0.0;
	for(int i = 0; i < GA_TARGET.length(); i++){
		a += abs(int(value[i] - GA_TARGET[i]));
	}
	return a;
}

void mate()
{
	int esize = int(GA_POPSIZE * GA_ELITRATE);
	for(int i = 0; i < esize; i++)
		buffer[i] = population[i];

	for(int i = esize; i < GA_POPSIZE; i++) {
		int i1 = rand() % (GA_POPSIZE / 4);
		int i2 = rand() % (GA_POPSIZE / 4);
		int spos = rand() % (GA_TARGET.length());
		buffer[i] = population[i1].substr(0, spos) + population[i2].substr(spos, GA_TARGET.length());
		if((rand() % 100) < GA_MUTATIONRATE) {
			int pos = rand() % GA_TARGET.length() - 1;
			buffer[i] = buffer[i].substr(0, pos) + char((rand() % 90) + 32) + buffer[i].substr(pos + 1, GA_TARGET.length());
		}
	}
}

void pointerSwap()
{
	std::vector<std::string>* temp = &population;
	*&population = *&buffer;
	*&buffer = *temp;
}

void CountingSort()
{
	std::vector<std::string> sorted(GA_POPSIZE);
	const int char_range = 122 - 32;
	const int largest_fitness = char_range * GA_TARGET.length();
	int sort_count[largest_fitness];
	for(int i = 0 ; i < largest_fitness; ++i) sort_count[i] = 0;
	for(int i = 0; i < GA_POPSIZE; ++i) sort_count[fitness(population[i])]++;
	int total = 0;
	int cur_count = 0;
	for(int i = 0; i < sizeof(sort_count) / sizeof(int); ++i) {
		cur_count = sort_count[i];
		sort_count[i] = total;
		total += cur_count;
	}
	for(int i = 0; i < GA_POPSIZE; ++i) {
		sorted[sort_count[fitness(population[i])]] = population[i];
		sort_count[fitness(population[i])]++;
	}
	population = sorted;
}

int main()
{
	InitializeAll();
	while(true) {
		CountingSort();
		std::cout << fitness(population[0]) << " " << population[0] << std::endl;
		if(fitness(population[0]) == 0) break;
		mate();
		pointerSwap();
	}
	return 0;
}

/* Old stuff maybe to revisit later
void swap() //managed code style swap
{
	std::vector<std::string> temp;
	for(auto& s : population)
		temp.push_back(s);
	for(int i = 0; i < buffer.size(); ++i)
		population[i] = buffer[i];
	for(int i =0; i < temp.size(); ++i)
		buffer[i] = temp[i];
}

bool _fitnessSort(std::string const& a, std::string const& b)
{
	return fitness(a) < fitness(b);
}

inline void SortByFitness()
{
	sort(population.begin(), population.end(), _fitnessSort);
}*/
