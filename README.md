# Projeto RA2 Funcional: Sistema de Inventário Haskell

Disciplina: Programação Lógica e Funcional

Professor: Frank Coelho de Alcantara

Alunos: 

- Cecília Lucchesi Mardegan (usuário: [ceciLcchM](https://github.com/ceciLcchM))
- Christine von Schmalz (usuário: [cvschmalz](https://github.com/cvschmalz))
- Erick Maestri de Souza (usuário: [ErickMS18](https://github.com/ErickMS18))

## Descrição do Projeto

Este projeto implementa um sistema de inventário em Haskell, que permite:

- Adicionar itens  
- Remover itens  
- Atualizar quantidade de itens
- Vender itens (reduzir o estoque)  
- Listar inventário  
- Registrar todas as operações automaticamente
- Visualizar relatórios (erros, item mais movimentado, e histórico por ID)

O sistema utiliza arquivos externos para persistência de dados: o inventário é gravado em `Inventario.dat` e a auditoria é gravada em `Auditoria.log`.


## Como Executar o Projeto

Este programa foi feito no Online GDB, conforme exigido na especificação.

### Link:
  https://onlinegdb.com/73rX3v0Uw

### Executando no Online GDB
1. Acesse o link acima
2. Abra o arquivo principal (`main.hs`) no editor
3. Clique em "Run"
4. O programa iniciará exibindo o menu principal.

# Cenários de testes manuais

Seguem abaixo os testes feitos conforme a especificação.

## Cenário 1: Persistência de Estado (Sucesso)

### Passo 1: Iniciar o programa (sem arquivos de dados).
![iniciando o programa sem arquivos](imagensHaskell/teste1.1.png)

### Passo 2: Adicionar 3 itens.
![adicionando 3 itens](teste1.2.1.png)
![adicionando 3 itens](./teste1.2.2.png)

### Passo 3: Fechar o programa.
![fechando o programa](imagensHaskell/teste1.3.png)

### Passo 4: Verificar se os arquivos Inventario.dat e Auditoria.log foram criados.
![verificando Inventario.dat](imagensHaskell/teste1.4.1.png)
![verificando Auditoria.log](imagensHaskell/teste1.4.2.png)

### Passo 5: Reiniciar o programa.
![reiniciando o programa](imagensHaskell/teste1.5.png)

### Passo 6: Executar um comando de "listar" (a ser criado) ou verificar se o estado carregado em memória contém os 3 itens.
![comando listar](imagensHaskell/teste1.6.png)



## Cenário 2: Erro de Lógica (Estoque Insuficiente)

### Passo 1: Adicionar um item com 10 unidades (ex: "teclado").
![adicionando um item com 10 uniddades](imagensHaskell/teste2.1.png)

### Passo 2: Tentar remover 15 unidades desse item.
![tentativa de remover 15 unidades do item](imagensHaskell/teste2.2.png)

### Passo 3: Verificar se o programa exibiu uma mensagem de erro clara.
![exibição da mensagem de erro clara](imagensHaskell/teste2.3.png)

### Passo 4: Verificar se o Inventario.dat (e o estado em memória) ainda mostra 10 unidades.
![comando listar mostrando 10 unidades](imagensHaskell/teste2.4.2.png)
![Inventario.dat mostrando 10 unidades](imagensHaskell/teste2.4.png)

### Passo 5: Verificar se o Auditoria.log contém uma LogEntry com StatusLog (Falha ...).
![Auditoria.log com uma LogEntry com StatusLog Falha](imagensHaskell/teste2.5.1.png)
![Auditoria.log com uma LogEntry com StatusLog Falha](imagensHaskell/teste2.5.2.png)



## Cenário 3: Geração de Relatório de Erros

### Passo 1: Após executar o Cenário 2, executar o comando report.
![comando report](imagensHaskell/teste3.1.png)

### Passo 2: Verificar se o relatório gerado (especificamente pela função logsDeErro) exibe a entrada de log referente à falha registrada no Cenário 2 (a tentativa de remover estoque insuficiente).
![entrada de log referente à falha registrada no cenario 2](imagensHaskell/teste3.2.png)



