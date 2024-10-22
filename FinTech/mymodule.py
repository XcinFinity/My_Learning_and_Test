def sum2n(n,m):
    '''
    To calculate the sum of 1^m + 2^m + ... + n^m
    :param n: the upper limit of the sum
    '''
    total = 0
    for i in range(1, n+1):
        total = total + i**m

    return total

def encode(name):
    '''
    Encode the name by shifting each letter by 1
    :param name: the name to be encoded
    '''
    en_str = ''

    for c in name:
        if c.isalpha():
            if c == 'Z':
                en_str = en_str + 'A'
            elif c == 'z':
                en_str = en_str + 'a'
            else:
                en_str = en_str + chr(ord(c) + 1)
        else:
            en_str = en_str + c

    print(f'The encode of {name} is {en_str}')

    return en_str

def decode(de_name):
    '''
    Decode the name by shifting each letter by 1
    :param de_name: the name to be decoded
    '''
    de_str = ''

    for c in de_name:
        if c.isalpha():
            if c == 'A':
                de_str = de_str - 'Z'
            elif c == 'a':
                de_str = de_str - 'z'
            else:
                de_str = de_str + chr(ord(c) - 1)
        else:
            de_str = de_str + c

    return de_str