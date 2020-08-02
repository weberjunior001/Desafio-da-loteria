      $set sourceformat"free"
      *>Divisão de identificação do programa
       Identification Division.
       Program-id. "Program1".
       Author. "Anderson Weber Junior".
       Installation. "PC".
       Date-written. 22/07/2020.
       Date-compiled. 22/07/2020.



      *>Divisão para configuração do ambiente
       Environment Division.
       Configuration Section.
           special-names. decimal-point is comma.

      *>-----Declaração dos recursos externos
       Input-output Section.
       File-control.
       I-O-Control.

      *>Declaração de variáveis
       Data Division.

      *>----Variaveis de arquivos
       File Section.


      *>----Variaveis de trabalho
       working-storage section.

       01  aposta occurs 10.
           05  numero                              pic 9(02).

       01  sorteado.
           05  sorteio                             pic 9(02) occurs 6.

       01  aposta-ctrl.
           05  numero-ctrl                         pic 9(02).
           05  sorteio-ctrl                        pic 9(02).
           05  controle-ctrl                       pic 9(01).

       77  controle-troca                          pic x(1).
           88  trocou                              value "t".
           88  nao_trocou                          value "n".

       77  controle                                pic 9(09).
       77  ind1                                    pic 9(02).
       77  qnt_numero                              pic 9(02).
       77  semente                                 pic 9(08).
       77  num_random                              pic 9(02)V9999.
       77  ind2                                    pic 9(02).

      *>----Variaveis para comunicação entre programas
       linkage section.


      *>----Declaração de tela
       screen section.

      *>Declaração do corpo do programa
       procedure Division.

           perform inicializa.
           perform processamento.
           perform finaliza.

      *>Inicilizacao de variaveis, abertura de arquivos
      *>procedimentos que serao realizados apenas uma vez
       inicializa section.
      *>inicialização de variaveis
           move 0 to controle
           move 0 to qnt_numero
           move 1 to ind1
           move 0 to controle-ctrl
           move 0 to sorteio-ctrl

           move 0 to numero(10)
           move 0 to numero(9)
           move 0 to numero(8)
           move 0 to numero(7)
           move 0 to numero(6)
           move 0 to numero(5)
           move 0 to numero(4)
           move 0 to numero(3)
           move 0 to numero(2)
           move 0 to numero(1)

           .
       inicializa-exit.
           exit.

      *>construçao do laço principal (menu) ou regra de negócio
       processamento section.

      *>continuara performando até que escolha um numero entre 6 e 10
           perform until qnt_numero >= 6 and qnt_numero <= 10
               display "Quantos numeros gostaria de apostar?(6 - 10)"
               accept qnt_numero
           end-perform

      *>Ira performar ate que o ind(referente ao numero de apostas) for maior que o numero de apostas desejadas
           perform varying ind1 from 1 by 1 until ind1 > qnt_numero

               display "Adicione o " ind1 " numero da aposta:"
               accept numero-ctrl

      *>teste de repetição, para conferir que não aposte o mesmo numero mais de uma vez
               perform teste-repeticao-aposta
               move numero-ctrl to numero(ind1)

               display erase

           end-perform

      *>performar ate que o a quantidade de numeros do sorteio iguais aos numeros de apostas seja igual a 6
           perform until controle-ctrl = 6
               move 0 to sorteio(6)
               move 0 to sorteio(5)
               move 0 to sorteio(4)
               move 0 to sorteio(3)
               move 0 to sorteio(2)
               move 0 to sorteio(1)

               perform geradorrandom

               perform teste-ganhador

               display "Numeros sorteados: " sorteio(1) "-" sorteio(2) "-" sorteio(3) "-" sorteio(4) "-" sorteio(5) "-"
               sorteio(6)
               display "Ja foi realizado " controle " sorteios"
               accept numero-ctrl
           end-perform

      *>caso seja igual a seis, significa que acertou os 6 numeros, ganhou na loteria
           if controle-ctrl = 6 then
               display "Parabens, dps de " controle " tentativas, voce ganhou"
           end-if
           .
       processamento-exit.
           exit.
      *>--------------------------------------------teste-repeticao----
       teste-repeticao-aposta section.

      *> nesse teste garante q a pessoa nao adicione dois numero iguais, e nem numero fora do intervalo de 1 e 60
           perform varying ind1 from 1 by 1 until numero(ind1) = 0

               *> intervalo de 1 a 60
               if numero-ctrl > 60 or numero-ctrl < 1 then
                   display "Voce adicionou um numero fora do intervalo"
                   display "Adicione outro numero"
                   accept numero-ctrl
               end-if
               *> numeros repetidos
               if numero-ctrl = numero(ind1) then

                   display "Voce adicionou um numero repetido"
                   display "Adicione outro numero"
                   accept numero-ctrl
               end-if

           end-perform
           .
       teste-repeticao-aposta-exit.
           exit.
      *>----------------------------------------------------------------

      *>--------------------------------------------geradorrandom-------
       geradorrandom section.
      *>perfomar ate que sorteie 6 numeros
           perform varying ind2 from 1 by 1 until ind2 > 6

           *> pega a semente do horario
               accept semente from time
               compute semente = (semente + (ind2 * ind1)) * (semente * (qnt_numero - ind2)) / sorteio-ctrl
           *> computa um numero aleatorio a partir da semente
               compute num_random = function random(semente)
           *> multiplica por 60 para que o numero esteja no intervalo desejado
               multiply num_random by 60 giving sorteio-ctrl

      *>teste de repetição do sorteio pra que nao tenha o mesmo numero entre os sorteados
               perform teste-repeticao-sorteio
               move sorteio-ctrl to sorteio(ind2)
           end-perform
      *> variavel para controlar quantas vezes foram sorteados
           add 1 to controle.
           .
       geradorrandom-exit.
           exit.
      *>----------------------------------------------------------------

      *>--------------------------------------------teste-repeticao----
       teste-repeticao-sorteio section.
      *> teste de repetição do sorteio usando o metodo bolha
           set nao_trocou to true
           perform varying ind2 from 1 by 1 until sorteio(ind2) = 0 or trocou

               if sorteio-ctrl = sorteio(ind2) then
                   compute ind2 = ind2 - 1
                   set trocou to true
               end-if

           end-perform
           .
       teste-repeticao-sorteio-exit.
           exit.
      *>----------------------------------------------------------------


      *>--------------------------------------------teste-ganhador ----
       teste-ganhador section.
      *>reinicia o valor de controle pra definir o vencedor
           move 0 to controle-ctrl
      *>metodo bolha para comparar cada numero sorteado com cada numero apostado
           perform varying ind2 from 1 by 1 until ind2 > 6

               perform varying ind1 from 1 by 1 until ind1 > qnt_numero

                   if sorteio(ind2) = aposta(ind1) then
                       add 1 to controle-ctrl
                   end-if

               end-perform

           end-perform
      *>reiniciar as variaveis associadas ao sorteio e a aposta
           move 1 to ind1
           move 1 to ind2
           .
       teste-ganhador-exit.
           exit.
      *>----------------------------------------------------------------

      *> fechamento de arquivos, procedimentos que são realizados uma
      *> unica vez no final do programa, impressao de relatorios
       finaliza section.

           stop run
           .
       finaliza-exit.
           exit.
