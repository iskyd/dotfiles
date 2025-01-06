{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "mattia";
  home.homeDirectory = "/home/mattia";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.11"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    pkgs.gping
    # pkgs.emacs29
    pkgs.dysk
    pkgs.git-graph
    pkgs.ripgrep
    pkgs.pass
    pkgs.fastfetch
    pkgs.fzf
    pkgs.fzy
    pkgs.pass
    pkgs.multimarkdown
    pkgs.lazydocker
    pkgs.difftastic
    pkgs.direnv
    pkgs.eza
    pkgs.zoxide
    pkgs.jujutsu
    pkgs.yazi
  ];

  programs.jujutsu = {
    enable = true;
    settings = {
      user.name = "iskyd";
      user.email = "iskyd@proton.me";
      signing.git = "991C76DCAA073547";
      signing.key = "991C76DCAA073547";
      signing.backend = "gpg";
      signing.sign-all = true;
    };
  };

  programs.git = {
	  enable = true;
    userName = "iskyd";
    userEmail = "iskyd@proton.me";
    signing.signByDefault = true;
    signing.key = "991C76DCAA073547";

    extraConfig = {
      rerere.enabled = true;
      rerere.autoUpdate = true;
      "url \"git@bitbucket.org:\"".insteadOf = "https://bitbucket.org";
      # "includeIf \"gitdir:/opt/projects/Conio/\"".path = "~/.config/.gitconfig-conio";
    };

    aliases = {
      amend = "commit --amend";
      authors = "!\"${pkgs.git}/bin/git log --pretty=format:%aN"
                   + " | ${pkgs.coreutils}/bin/sort"
                   + " | ${pkgs.coreutils}/bin/uniq -c"
                   + " | ${pkgs.coreutils}/bin/sort -rn\"";
    };

    ignores = [
      "#*#"
      "*~"
    ];

    includes = [
      {
        path = "~/.config/.gitconfig-conio";
        condition = "gitdir:/opt/projects/Conio/";
      }
    ];
      
  };

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file.".config/.gitconfig-conio".text = ''
                [user]
                        name = "mattia"
                        email = "mattia.careddu@conio.com"
                        signingKey =  
                        signbydefault = false
                [commit]
                        gpgSign = false
                [tag]
                        gpgSign = false
                [url "git@bitbucket.org:"]
                        insteadOf = https://bitbucket.org
                
  '';

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. If you don't want to manage your shell through Home
  # Manager then you have to manually source 'hm-session-vars.sh' located at
  # either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/mattia/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    EDITOR = "emacs -nw -q --load ~/.emacs.d/init_editor.el";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
