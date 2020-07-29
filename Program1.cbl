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

       01  sorteado occurs 6.
           05  sorteio                             pic 9(02).

       01  aposta-ctrl.
           05  numero-ctrl                         pic 9(02).
           05  sorteio-ctrl                        pic 9(02).
           05  controle-ctrl                       pic 9(01).

       77  controle-troca                          pic x(1).
           88  trocou                              value "t".
           88  nao_trocou                          value "n".

       77  controle                                pic 9(09).
       77  ind1                                    pic 9(02).
       77  qnt_numero                              pic 9(01).
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
           move 0 to controle
           move 0 to qnt_numero
           move 1 to ind1
           move 0 to controle-ctrl

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

           perform until qnt_numero >= 6 and qnt_numero <= 10
               display "Quantos numeros gostaria de apostar?(6 - 10)"
               accept qnt_numero
           end-perform

           perform varying ind1 from 1 by 1 until ind1 > qnt_numero

               display "Adicione o " ind1 " numero da aposta:"
               accept numero-ctrl

               perform teste-repeticao-aposta
               move numero-ctrl to numero(ind1)

               display erase

           end-perform
           perform until controle-ctrl = 6
               move 0 to sorteio(6)
               move 0 to sorteio(5)
               move 0 to sorteio(4)
               move 0 to sorteio(3)
               move 0 to sorteio(2)
               move 0 to sorteio(1)

               perform geradorrandom

               perform teste-ganhador

               display "Já foi realizado " controle " sorteios"
               accept numero-ctrl
           end-perform

           if controle-ctrl = 6 then
               display "Parabens, dps de " controle " tentativas, voce ganhou"
           .
       processamento-exit.
           exit.
      *>--------------------------------------------teste-repeticao----
       teste-repeticao-aposta section.

           perform varying ind1 from 1 by 1 until numero(ind1) = 0

               if numero-ctrl > 60 or numero-ctrl < 1 then
                   display "Voce adicionou um numero fora do intervalo"
                   display "Adicione outro numero"
                   accept numero-ctrl
               end-if
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
           perform varying ind2 from 1 by 1 until ind2 > 6
               accept semente from time
               compute num_random = function random(semente)
               multiply num_random by 60 giving sorteio-ctrl

               perform teste-repeticao-sorteio
               move sorteio-ctrl to sorteio(ind2)
           end-perform
           add 1 to controle.
           .
       geradorrandom-exit.
           exit.
      *>----------------------------------------------------------------

      *>--------------------------------------------teste-repeticao----
       teste-repeticao-sorteio section.
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


      *>--------------------------------------------teste-repeticao----
       teste-ganhador section.
           perform varying ind2 from 1 by 1 until ind2 > 6

               perform varying ind1 from 1 by 1 until ind1 > qnt_numero

                   if sorteio(ind2) = aposta(ind1) then
                       add 1 to controle-ctrl
                   end-if

               end-perform
           end-perform
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
