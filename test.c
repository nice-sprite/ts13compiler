#include <stdio.h>
typedef char bool;
int main() {
	int INPUT1;
	int COUNTER;
	scanf("%d", &INPUT1);

	COUNTER = 0;
	while (COUNTER < INPUT1) {
		COUNTER = COUNTER + 1;
		if (COUNTER % 2 == 0) {
			printf("%d\n", COUNTER);

		}


	}

}