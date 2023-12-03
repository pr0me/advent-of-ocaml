with open('clean_games.txt', 'w+') as f1:
    with open('games.txt', 'r') as f2:
        new = ""
        og = f2.read()
        print(og)
        for line in og.split('\n'):
            new_line = line.split(': ')[1:]
            new_line = ''.join(new_line)

            f1.write(new_line + '\n')