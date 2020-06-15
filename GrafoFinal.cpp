#include <stdio.h>
#include <stdlib.h>
#define INF 99999

struct grafo{
    int eh_ponderado;
    int nro_vertices;
    int grau_max;
    int** arestas;
    float** pesos;
    int* grau;
};

typedef struct grafo Grafo;

Grafo* cria_Grafo(int nro_vertices, int grau_max, int eh_ponderado){
    Grafo *gr;
    gr = (Grafo*) malloc(sizeof(struct grafo));
    if(gr != NULL){
        int i;
        gr->nro_vertices = nro_vertices;
        gr->grau_max = grau_max;
        gr->eh_ponderado = (eh_ponderado != 0)?1:0;
        gr->grau = (int*) calloc(nro_vertices,sizeof(int));

        gr->arestas = (int**) malloc(nro_vertices * sizeof(int*));
        for(i=0; i<nro_vertices; i++)
            gr->arestas[i] = (int*) malloc(grau_max * sizeof(int));

        if(gr->eh_ponderado){
            gr->pesos = (float**) malloc(nro_vertices * sizeof(float*));
            for(i=0; i<nro_vertices; i++)
                gr->pesos[i] = (float*) malloc(grau_max * sizeof(float));
        }
        
    }
    return gr;
}

void libera_Grafo(Grafo* gr){
    if(gr != NULL){
        int i;
        for(i=0; i<gr->nro_vertices; i++)
            free(gr->arestas[i]);
        free(gr->arestas);

        if(gr->eh_ponderado){
            for(i=0; i<gr->nro_vertices; i++)
                free(gr->pesos[i]);
            free(gr->pesos);
        }
        free(gr->grau);
        free(gr);
    }
}

int insereAresta(Grafo* gr, int orig, int dest, int eh_digrafo, float peso){
    if(gr == NULL)
        return 0;
    if(orig < 0 || orig >= gr->nro_vertices)
        return 0;
    if(dest < 0 || dest >= gr->nro_vertices)
        return 0;

    gr->arestas[orig][gr->grau[orig]] = dest;
    if(gr->eh_ponderado)
        gr->pesos[orig][gr->grau[orig]] = peso;
    gr->grau[orig]++;

    if(eh_digrafo == 0)
        insereAresta(gr,dest,orig,1,peso);
    return 1;
}

void imprimirGrafo(Grafo *gr){
    if(gr == NULL)
        return;

    int i, j;
    for(i=0; i < gr->nro_vertices; i++){
        printf("%d: ", i);
        for(j=0; j < gr->grau[i]; j++){
            if(gr->eh_ponderado)
                printf("%d(%.2f), ", gr->arestas[i][j], gr->pesos[i][j]);
            else
                printf("%d, ", gr->arestas[i][j]);
        }
        printf("\n");
    }
}

int MenorDistancia(float *dist, int *visitado, int NV){
    int i, menor = -1, primeiro = 1;
    for(i=0; i < NV; i++){
        if(dist[i] >= 0 && visitado[i] == 0){
            if(primeiro){
                menor = i;
                primeiro = 0;
            }else{
                if(dist[menor] > dist[i])
                    menor = i;
            }
        }
    }
    return menor;
}

void Dijkstra(Grafo *gr, int ini, int *ant, float *dist){
    int i, cont, NV, ind, *visitado, vert;
    cont = NV = gr->nro_vertices;
    visitado = (int*) malloc(NV * sizeof(int));
    for(i=0; i < NV; i++){
        ant[i] = -1;
        dist[i] = -1;
        visitado[i] = 0;
    }
    dist[ini] = 0;
    while(cont > 0){
        vert = MenorDistancia(dist, visitado, NV);
        
        if(vert == -1)
            break;

        visitado[vert] = 1;
        cont--;
        for(i=0; i<gr->grau[vert]; i++){
            ind = gr->arestas[vert][i];
            if(dist[ind] < 0){
               dist[ind] = dist[vert] + gr->pesos[vert][i];
               ant[ind] = vert;
            }else{
                if(dist[ind] > dist[vert] + 1){
                    dist[ind] = dist[vert] + gr->pesos[vert][i];
                    ant[ind] = vert;
                }
            }
        }
    }

    free(visitado);
}

void prim(Grafo *gr, int orig, int *pai){
	int i, j, dest, primeiro, nv = gr->nro_vertices;
	double menorpeso;
	
	for(i=0; i< nv; i++){
		pai[i] = -1;
	}
	pai[orig] = orig;
	
	while(1){
		primeiro = 1;
		for(i=0; i<nv; i++){
			if(pai[i] != -1){
				for(j=0; j<gr->grau[i]; j++){
					if(pai[gr->arestas[i][j]] == -1){
						if(primeiro){
							menorpeso = gr->pesos[i][j];
							orig = i;
							dest = gr->arestas[i][j];
							primeiro = 0;
						}else{
							if(menorpeso > gr->pesos[i][j]){
								menorpeso = gr->pesos[i][j];
								orig = i;
								dest = gr->arestas[i][j];
							}
						}
					}
				}
			}
		}
		if(primeiro == 1){
			break;
		}
		pai[dest] = orig;
	}
}

void kruskal(Grafo *gr, int orig, int *pai){
	int i, j, dest, primeiro, nv=gr->nro_vertices;
	double menorpeso;
	int *a = (int*)malloc(nv*sizeof(int));
	for(i=0; i<nv; i++){
		a[i] = i;
		pai[i] = -1;
	}
	pai[orig] = orig;
	while(1){
		primeiro = 1;
		for(i=0; i<nv; i++){
			for(j=0; j<gr->grau[i]; j++){
				if(a[i] != a[gr->arestas[i][j]]){
					if(primeiro){
						menorpeso = gr->pesos[i][j];
						orig = i;
						dest = gr->arestas[i][j];
						primeiro = 0;
					}else{
						if(menorpeso > gr->pesos[i][j]){
							menorpeso = gr->pesos[i][j];
							orig = i;
							dest = gr->arestas[i][j];
						}
					}
				}		
			}
		}
		if(primeiro == 1){
			break;
		}
		if(pai[orig] == -1){
			pai[orig] = dest;
		}
		else{
			pai[dest] = orig;
		}
		
		for(i=0; i<nv; i++){
			if(a[i] == a[dest]){
				a[i] = a[orig];
			}
		}
	}
}

void printarsolucao(int **dist, Grafo *gr)
{
	int i,j;
    for (i = 0; i < gr->nro_vertices; i++)
    {
        for (j = 0; j < gr->nro_vertices; j++)
        {
            if (dist[i][j] == INF)
                printf("%7s", "INF");
            else
                printf ("%7d", dist[i][j]);
        }
        printf("\n");
    }
}

void floydWarshall(Grafo *gr)
{
    //printf("uolop\n");
	int **dist = (int**)malloc(gr->nro_vertices * sizeof(int*));
    //printf("uolop\n");
    int i, j, k;
    for (i = 0; i < gr->nro_vertices; i++){
        for (j = 0; j < gr->nro_vertices; j++){
            dist[i][j] = gr->arestas[i][j];
        	//printf("uolop\n");
		}
	}
	//printf("uolop\n");
 
    //printf("uolop\n");
    for (k = 0; k < gr->nro_vertices; k++)
    {
        for (i = 0; i < gr->nro_vertices; i++)
        {
            for (j = 0; j < gr->nro_vertices; j++)
            {
                if (dist[i][k] + dist[k][j] < dist[i][j])
                    dist[i][j] = dist[i][k] + dist[k][j];
            }
        }
    }
    //printf("uolop\n");
    printarsolucao(dist,gr);
}

int main(){
    int digrafo = 1; // se for digrafo: 1, senão: 0
    int i,j;
    int vorigem, vdestino, peso, nv, col;
    
    FILE *arquivo = fopen("grafo.txt", "r");

    if(arquivo == NULL){
        printf("Erro, nao foi possivel abrir o arquivo\n");
    }else{
        fscanf(arquivo, "%d %d\n", &nv, &col);
    }
    
    Grafo* gr = cria_Grafo(nv, col, 1);
    
    while((fscanf(arquivo, "%d %d %d\n", &vorigem, &vdestino, &peso))!= EOF ){
		insereAresta(gr, vorigem, vdestino, digrafo, peso);
    }

    fclose(arquivo);
	
	int vis[6];
	int ant[gr->nro_vertices];
	float dist[gr->nro_vertices];
	int pai[gr->nro_vertices];
	int dad[gr->nro_vertices];
	int orig;
	
	int op, op2;
	printf("[1] - Imprimir Grafo\n");
	printf("[2] - Dijkstra\n");
	printf("[3] - Prim\n");
	printf("[4] - Kruskal\n");
	printf("[5] - Floyd-Warshall\n");
	printf("[6] - Finalizar\n");
	paranaue:
	printf("Digite uma opcao: ");
	scanf("%d",&op);
	
	switch(op){
		case 1:
			printf("\nGrafo\n");
    		imprimirGrafo(gr);
			printf("\n\nDeseja continuar?\n");
			printf("[1] - Sim\n");
			printf("[2] - Nao\n");
			scanf("%d",&op2);
			if(op2==1){
				goto paranaue;
			}
			break;
			
		case 2:
			printf("\nDijkstra\n");
			printf("Qual o vertice de origem? ");
			scanf("%d",&orig);
		    Dijkstra(gr, orig, ant, dist);
		    for(i=0; i<gr->nro_vertices; i++){
				printf("vertice: %d | vertice anterior: %d | distancia da origem (custo): %.1f\n",i,ant[i],dist[i]);
			}
			printf("\n\nDeseja continuar?\n");
			printf("[1] - Sim\n");
			printf("[2] - Nao\n");
			scanf("%d",&op2);
			if(op2==1){
				goto paranaue;
			}
			break;
			
		case 3:
			printf("\nPrim\n");
			printf("Qual o vertice de origem? ");
			scanf("%d",&orig);
			prim(gr, orig, pai);
			for(i=0; i<gr->nro_vertices; i++){
				printf("Pai do vertice %d: %d\n", i, pai[i]);
			}
			printf("\n\nDeseja continuar?\n");
			printf("[1] - Sim\n");
			printf("[2] - Nao\n");
			scanf("%d",&op2);
			if(op2==1){
				goto paranaue;
			}
			break;
			
		case 4:
			printf("\nKruskal\n");
			printf("Qual o vertice de origem? ");
			scanf("%d",&orig);
			kruskal(gr, orig, dad);
			for(i=0; i<gr->nro_vertices; i++){
				printf("Pai do vertice %d: %d\n", i, dad[i]);
			}
			printf("\n\nDeseja continuar?\n");
			printf("[1] - Sim\n");
			printf("[2] - Nao\n");
			scanf("%d",&op2);
			if(op2==1){
				goto paranaue;
			}
			break;
			
		case 5:
			printf("\nFloyd-Warshall\n");
			floydWarshall(gr);
			printf("\n\nDeseja continuar?\n");
			printf("[1] - Sim\n");
			printf("[2] - Nao\n");
			scanf("%d",&op2);
			if(op2==1){
				goto paranaue;
			}
			break;
			
		case 6:
			printf("\nFinalizado!\n\n");
			break;
			
		default:
			printf("\nEscolha uma opcao valida!\n\n");
			goto paranaue;
	}
	
    libera_Grafo(gr);
    printf("\n\n");
    
    system("pause");
    return 0;
}

